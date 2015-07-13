
/*
  Library interface to FCEUX. This is an attempt to proper encapsulation
  of the emulator so that there can be multiple instances at once and
  they can run in separate threads, but I haven't succeded at that yet;
  if you create multiple instances of this object they will trample on
  each other and cause undefined behavior.
*/

#ifndef __EMULATOR_H
#define __EMULATOR_H

#include <vector>
#include <string>
#include "types.h"

extern "C" {

using namespace std;

struct FCEUGI;

uint32 joydata = 0;
// Initializes the emulator
void Create(char* romfile);
void Destroy(void);
unsigned long Save(uint8 **out);
void Load(uint8 *in);

// Make one emulator step with the given input.
// Bits from MSB to LSB are
//    RLDUTSBA (Right, Left, Down, Up, sTart, Select, B, A)
//
// Consider StepFull if you want video or CachingStep if you
// are doing search and might execute this same step again.
void Step(uint8);
uint8* GetMemory(void);

// Fancy stuff.

// Same, but run the video and sound code as well. This is slower,
// but allows calling GetImage and GetSound.
void StepFull(uint8);

// Get image. StepFull must have been called to produce this frame,
// or else who knows what's in there? Size is 256 x 256 pixels,
// 4 color channels (bytes) per pixel in RGBA order, though only
// 240 pixels high contain anything interesting.
uint8* GetImage();

// Get sound. StepFull must have been called to produce this wave.
// The result is a vector of signed 16-bit samples, mono.
void GetSound(vector<int16> *wav);

// Returns 64-bit checksum (based on MD5, endianness-dependent)
// of RAM (only). Note there are other important bits of state.
uint64 RamChecksum();
// Same, of the RGBA image. We only look at 240 scanlines here.
uint64 ImageChecksum();

}

#endif
