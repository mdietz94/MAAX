
#include "emulator.h"

#include <algorithm>
#include <string>
#include <vector>
#include <zlib.h>
#include <unordered_map>

#include "driver.h"
#include "fceu.h"
#include "types.h"
#include "utils/md5.h"
#include "version.h"
#include "state.h"
#include "sound.h"
#include "palette.h"
#include "input.h"
#include "ppu.h"

// The current contents of the screen; part of the "API".
// extern uint8 *XBuf, *XBackBuf;

uint8* GetMemory() {
  uint8* mem = (uint8*)malloc(0x800);
  memcpy(mem, RAM, 0x800);
  return mem;
}

static inline uint64 MD5ToChecksum(const uint8 digest[16]) {
  uint64 res = 0ULL;
  for (int i = 0; i < 8; i++) {
    res <<= 8;
    res |= 255 & digest[i];
  }
  return res;
}

uint64 RamChecksum() {
  md5_context ctx;
  md5_starts(&ctx);
  md5_update(&ctx, RAM, 0x800);
  uint8 digest[16];
  md5_finish(&ctx, digest);
  return MD5ToChecksum(digest);
}

uint64 ImageChecksum() {
  md5_context ctx;
  md5_starts(&ctx);
  uint8 *img = GetImage();
  md5_update(&ctx, img, 256 * 240 * 4);
  uint8 digest[16];
  md5_finish(&ctx, digest);
  return MD5ToChecksum(digest);
}

/**
 * Initialize all of the subsystem drivers: video, audio, and joystick.
 */
int DriverInitialize(FCEUGI *gi) {
  // Used to init video. I think it's safe to skip.

  // Here we initialized sound. Assuming it's safe to skip,
  // because of an early return if config turned it off.

  // Used to init joysticks. Don't care about that.

  // No fourscore support.
  // eoptions &= ~EO_FOURSCORE;

  fceulib__sound.FCEUI_InitSound();

  // Why do both point to the same joydata? -tom
  fceulib__input.FCEUI_SetInput(0, SI_GAMEPAD, &joydata, 0);
  fceulib__input.FCEUI_SetInput(1, SI_GAMEPAD, &joydata, 0);

  fceulib__input.FCEUI_SetInputFourscore(false);
  return 1;
}

/**
 * Loads a game, given a full path/filename.  The driver code must be
 * initialized after the game is loaded, because the emulator code
 * provides data necessary for the driver code (number of scanlines to
 * render, what virtual input devices to use, etc.).
 */
int LoadGame(const string &path) {
  FCEU_CloseGame();
  GameInfo = nullptr;

  if (!FCEUI_LoadGame(path.c_str(), 1)) {
    return 0;
  }

  // Here we used to do ParseGIInput, which allows the gameinfo
  // to override our input config, or something like that. No
  // weird stuff. Skip it.

  if (!DriverInitialize(GameInfo)) {
    return 0;
  }
	
  // Set NTSC (1 = pal). Note that cartridges don't contain this information
  // and some parts of the code tried to figure it out from the presence of
  // (e) or (pal) in the ROM's *filename*. Maybe should be part of the external
  // intface.
  FCEUI_SetVidSystem(GIV_NTSC);

  return 1;
}

void Destroy() {
  FCEU_CloseGame();
  GameInfo = nullptr;
  FCEUI_Kill();
}

void Create(char* romfileC) {
  // initialize the infrastructure
  string romfile (romfileC);
  int error = FCEUI_Initialize();
  if (error != 1) {
    fprintf(stderr, "Error initializing.\n");
  }

  // defaults
  // TODO(tom7): Make these compile-time constants inside of Palette rather
  // than state.
  static constexpr int ntsccol = 0, ntsctint = 56, ntschue = 72;
  fceulib__palette.FCEUI_SetNTSCTH(ntsccol, ntsctint, ntschue);

  // Set NTSC (1 = pal)
  FCEUI_SetVidSystem(GIV_NTSC);

  // Default.
  fceulib__ppu.FCEUI_DisableSpriteLimitation(1);

  // Load the game.
  if (1 != LoadGame(romfile.c_str())) {
    fprintf(stderr, "Couldn't load [%s]\n", romfile.c_str());
  }
}

// Make one emulator step with the given input.
// Bits from MSB to LSB are
//    RLDUTSBA (Right, Left, Down, Up, sTart, Select, B, A)
void Step(uint8 data) {
  // Limited ability to skip video and sound.
  static constexpr int SKIP_VIDEO_AND_SOUND = 2;
  joydata = (uint32)data;

  // Emulate a single frame.
  int32 *sound;
  int32 ssize;
  FCEUI_Emulate(nullptr, &sound, &ssize, SKIP_VIDEO_AND_SOUND);
}

void StepFull(uint8 data) {

  // Run the video and sound as well.
  static constexpr int DO_VIDEO_AND_SOUND = 0;

  joydata = (uint32)data;
  // Emulate a single frame.
  // TODO: Remove these arguments, which we don't use.
  int32 *sound;
  int32 ssize;
  FCEUI_Emulate(nullptr, &sound, &ssize, DO_VIDEO_AND_SOUND);
}

uint8* GetImage() {
  uint8* img = (uint8*)malloc(256*256*4*sizeof(uint8));
  for (int y = 0; y < 256; y++) {
    for (int x = 0; x < 256; x++) {
      uint8 r, g, b;

      // XBackBuf? or XBuf?
      fceulib__palette.FCEUD_GetPalette(XBuf[(y * 256) + x], &r, &g, &b);

      img[y * 256 * 4 + x * 4 + 0] = r;
      img[y * 256 * 4 + x * 4 + 1] = g; // XBackBuf[(y * 256) + x] << 4;
      img[y * 256 * 4 + x * 4 + 2] = b; // XBuf[(y * 256) + x] << 4;
      img[y * 256 * 4 + x * 4 + 3] = 0xFF;
    }
  }
  return img;
}

void GetSound(vector<int16> *wav) {
  wav->clear();
  int32 *buffer = nullptr;
  int samples = fceulib__sound.GetSoundBuffer(&buffer);
  if (buffer == nullptr) {
    fprintf(stderr, "No sound buffer?\n");
    abort();
  }

  wav->resize(samples);
  for (int i = 0; i < samples; i++) {
    (*wav)[i] = (int16)buffer[i];
  }
}

unsigned long Save(uint8** out_ptr) {
  vector<uint8> data;
  FCEUSS_SaveRAW(&data);
  (*out_ptr) = (uint8*)malloc(data.size());
  memcpy((*out_ptr), data.data(), data.size());
  return data.size();
}

void Load(uint8 *in) {
  vector<uint8> v = vector<uint8>(in, in + sizeof(in) / sizeof(uint8));
  if (!FCEUSS_LoadRAW(&v)) {
    fprintf(stderr, "Couldn't restore from state\n");
    abort();
  }
}
