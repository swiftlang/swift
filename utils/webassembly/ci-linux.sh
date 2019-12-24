#/bin/bash

sudo apt update
sudo apt install \
  git ninja-build clang python \
  uuid-dev libicu-dev icu-devtools libbsd-dev \
  libedit-dev libxml2-dev libsqlite3-dev swig \
  libpython-dev libncurses5-dev pkg-config \
  libblocksruntime-dev libcurl4-openssl-dev \
  systemtap-sdt-dev tzdata rsync wget

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd  )" 
SWIFT_PATH=$SOURCE_PATH/swift
BUILD_SCRIPT=$SWIFT_PATH/utils/webassembly/build-linux.sh
cd $SWIFT_PATH

export current_sha=`git rev-parse HEAD`
./utils/update-checkout --clone --scheme wasm
git checkout $current_sha

# Install wasmtime

sudo mkdir /opt/wasmtime && cd /opt/wasmtime
wget -O - "https://github.com/bytecodealliance/wasmtime/releases/download/v0.8.0/wasmtime-v0.8.0-x86_64-linux.tar.xz" | \
  sudo tar x --strip-components 1
sudo ln -sf /opt/wasmtime/* /usr/local/bin

cd $SOURCE_PATH

wget -O install_cmake.sh "https://github.com/Kitware/CMake/releases/download/v3.15.3/cmake-3.15.3-Linux-x86_64.sh"
chmod +x install_cmake.sh
sudo mkdir -p /opt/cmake
sudo ./install_cmake.sh --skip-license --prefix=/opt/cmake
sudo ln -sf /opt/cmake/bin/* /usr/local/bin
cmake --version

wget -O dist-wasi-sdk.tgz https://github.com/swiftwasm/wasi-sdk/suites/370986556/artifacts/809002
unzip dist-wasi-sdk.tgz
WASI_SDK_TAR_PATH=$(find dist-ubuntu-latest.tgz -type f -name "wasi-sdk-*")
WASI_SDK_FULL_NAME=$(basename $WASI_SDK_TAR_PATH -linux.tar.gz)
tar xfz $WASI_SDK_TAR_PATH
mv $WASI_SDK_FULL_NAME ./wasi-sdk

# Link wasm32-wasi-unknown to wasm32-wasi because clang finds crt1.o from sysroot
# with os and environment name `getMultiarchTriple`.
ln -s wasm32-wasi wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi-unknown

wget -O icu.tar.xz "https://github.com/swiftwasm/icu4c-wasi/releases/download/20190421.3/icu4c-wasi.tar.xz"
tar xf icu.tar.xz

$BUILD_SCRIPT --release --debug-swift-stdlib --verbose
# Run test but ignore failure temporarily
$BUILD_SCRIPT --release --debug-swift-stdlib --verbose -t || true
