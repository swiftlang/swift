#!/bin/bash

set -ex

if [[ "$(cat /etc/system-release)" == *"Amazon Linux release 2"* ]]; then
  sudo yum -y install   \
    clang               \
    curl-devel          \
    gcc-c++             \
    git                 \
    glibc-static        \
    libbsd-devel        \
    libedit-devel       \
    libicu-devel        \
    libuuid-devel       \
    libxml2-devel       \
    ncurses-compat-libs \
    ncurses-devel       \
    ninja-build         \
    pexpect             \
    pkgconfig           \
    procps-ng           \
    python              \
    python-devel        \
    python-pkgconfig    \
    python-six          \
    python3-devel       \
    rsync               \
    sqlite-devel        \
    swig                \
    tzdata              \
    uuid-devel          \
    wget                \
    which

  if [ ! -e /usr/local/bin/ninja ]; then
    sudo ln -s /usr/bin/ninja-build /usr/local/bin/ninja
  fi
else
  sudo apt update

  if [ $(grep RELEASE /etc/lsb-release) == "DISTRIB_RELEASE=18.04" ]; then
    sudo apt install -y \
      git ninja-build clang-10 python python-six \
      uuid-dev libicu-dev icu-devtools libbsd-dev \
      libedit-dev libxml2-dev libsqlite3-dev swig \
      libpython-dev libncurses5 libncurses5-dev pkg-config \
      libblocksruntime-dev libcurl4-openssl-dev \
      make systemtap-sdt-dev tzdata rsync wget llvm-10 zip unzip
    sudo ln -s -f /usr/bin/clang-10 /usr/bin/clang
    sudo ln -s -f /usr/bin/clang++-10 /usr/bin/clang++
  elif [ $(grep RELEASE /etc/lsb-release) == "DISTRIB_RELEASE=20.04" ]; then
    sudo apt install -y \
      git ninja-build clang python python-six \
      uuid-dev libicu-dev icu-devtools libbsd-dev \
      libedit-dev libxml2-dev libsqlite3-dev swig \
      libpython2-dev libncurses5 libncurses5-dev pkg-config \
      libblocksruntime-dev libcurl4-openssl-dev \
      make systemtap-sdt-dev tzdata rsync wget llvm zip unzip
  elif [ $(grep RELEASE /etc/lsb-release) == "DISTRIB_RELEASE=22.04" ]; then
    sudo apt install -y \
      git ninja-build clang python3 python-six \
      uuid-dev libicu-dev icu-devtools libbsd-dev \
      libedit-dev libxml2-dev libsqlite3-dev swig \
      libpython2-dev libncurses5 libncurses5-dev pkg-config \
      libblocksruntime-dev libcurl4-openssl-dev \
      make systemtap-sdt-dev tzdata rsync wget llvm zip unzip
  else
    echo "Unknown Ubuntu version"
    exit 1
  fi
  sudo apt clean
fi

SOURCE_PATH="$( cd "$(dirname $0)/../../../.." && pwd )" 
SWIFT_PATH=$SOURCE_PATH/swift
cd $SWIFT_PATH

./utils/update-checkout --clone --scheme wasm --skip-repository swift

# Install wasmer
# FIXME: Wasmer doesn't support linux-aarch64, consider using a different WASI-compatible runtime.
if [ "$(uname -m)" != "aarch64" ]; then
  if [ ! -e ~/.wasmer/bin/wasmer ]; then
    curl https://get.wasmer.io -sSfL | sh -s "2.1.1"
  fi
fi

cd $SOURCE_PATH

if [ -z $(which cmake) ]; then
  wget -O install_cmake.sh "https://github.com/Kitware/CMake/releases/download/v3.22.1/cmake-3.22.1-linux-$(uname -m).sh"
  chmod +x install_cmake.sh
  sudo mkdir -p /opt/cmake
  sudo ./install_cmake.sh --skip-license --prefix=/opt/cmake
  sudo ln -sf /opt/cmake/bin/* /usr/local/bin
fi

cmake --version

# Install sccache

if [ -z $(which sccache) ]; then
  sudo mkdir /opt/sccache && cd /opt/sccache
  wget -O - "https://github.com/mozilla/sccache/releases/download/v0.3.0/sccache-v0.3.0-$(uname -m)-unknown-linux-musl.tar.gz" | \
    sudo tar xz --strip-components 1
  sudo ln -sf /opt/sccache/sccache /usr/local/bin
fi
