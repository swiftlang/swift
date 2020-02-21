#/bin/bash

set -ex

sudo apt update
sudo apt install -y \
  git ninja-build clang python python-six \
  uuid-dev libicu-dev icu-devtools libbsd-dev \
  libedit-dev libxml2-dev libsqlite3-dev swig \
  libpython-dev libncurses5-dev pkg-config \
  libblocksruntime-dev libcurl4-openssl-dev \
  systemtap-sdt-dev tzdata rsync wget llvm zip unzip

SOURCE_PATH="$( cd "$(dirname $0)/../../../.." && pwd )" 
SWIFT_PATH=$SOURCE_PATH/swift
cd $SWIFT_PATH

./utils/update-checkout --clone --scheme wasm --skip-repository swift

# Install wasmtime

sudo mkdir /opt/wasmtime && cd /opt/wasmtime
wget -O - "https://github.com/bytecodealliance/wasmtime/releases/download/v0.8.0/wasmtime-v0.8.0-x86_64-linux.tar.xz" | \
  sudo tar Jx --strip-components 1
sudo ln -sf /opt/wasmtime/* /usr/local/bin

cd $SOURCE_PATH

wget -O install_cmake.sh "https://github.com/Kitware/CMake/releases/download/v3.15.3/cmake-3.15.3-Linux-x86_64.sh"
chmod +x install_cmake.sh
sudo mkdir -p /opt/cmake
sudo ./install_cmake.sh --skip-license --prefix=/opt/cmake
sudo ln -sf /opt/cmake/bin/* /usr/local/bin
cmake --version

wget -O dist-wasi-sdk.tgz.zip "https://github.com/swiftwasm/wasi-sdk/releases/download/0.2.0-swiftwasm/dist-ubuntu-latest.tgz.zip"
unzip dist-wasi-sdk.tgz.zip -d .
WASI_SDK_TAR_PATH=$(find . -type f -name "wasi-sdk-*")
WASI_SDK_FULL_NAME=$(basename $WASI_SDK_TAR_PATH -linux.tar.gz)
tar xfz $WASI_SDK_TAR_PATH
mv $WASI_SDK_FULL_NAME ./wasi-sdk

# Link wasm32-wasi-unknown to wasm32-wasi because clang finds crt1.o from sysroot
# with os and environment name `getMultiarchTriple`.
ln -s wasm32-wasi wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi-unknown

wget -O icu.tar.xz "https://github.com/swiftwasm/icu4c-wasi/releases/download/0.3.0/icu4c-wasi.tar.xz"
tar xf icu.tar.xz
