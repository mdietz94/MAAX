FROM debian:jessie
MAINTAINER Arthur Burkart <artburkart@gmail.com>

ENV DEBIAN_FRONTEND noninteractive
ENV LANG            C.UTF-8

COPY ./app /app

# INSTALL DEPENDENCIES
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list \
  && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286\
  && apt-get update \
  && apt-get install -y curl vim make libc6 libgmp10 libgmp-dev zlib1g-dev g++ xz-utils cabal-install-1.22 \
  && rm -rf /var/curllib/apt/lists/*

# INSTALL HASKELL
RUN curl -sSL https://www.haskell.org/ghc/dist/7.10.1/ghc-7.10.1-x86_64-unknown-linux-deb7.tar.xz \
  | tar -v -C /opt -xJ \
  && cd /opt/ghc-7.10.1 \
  && ./configure \
  && make install

ENV PATH /root/.cabal/bin:/opt/cabal/1.22/bin:$PATH

WORKDIR /app

RUN cabal update && cabal install

# SETUP EMULATOR
RUN cd tom7/fceulib \
  && make all \
  && cp libemulator.so /app/tom7/libemulator.so \
  && cp libemulator.so /app/dist/build/MAAX/libemulator.so
