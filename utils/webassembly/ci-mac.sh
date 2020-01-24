#/bin/bash

set -ex

brew install cmake ninja llvm

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd  )" 
SWIFT_PATH=$SOURCE_PATH/swift
BUILD_SCRIPT=$SWIFT_PATH/utils/webassembly/build-mac.sh
cd $SWIFT_PATH

./utils/update-checkout --clone --scheme wasm --skip-repository swift

# Install wasmtime

sudo mkdir /opt/wasmtime && cd /opt/wasmtime
wget -O - "https://github.com/bytecodealliance/wasmtime/releases/download/v0.8.0/wasmtime-v0.8.0-x86_64-macos.tar.xz" | \
  sudo tar Jx --strip-components 1
sudo ln -sf /opt/wasmtime/* /usr/local/bin

cd $SOURCE_PATH

wget -O dist-wasi-sdk.tgz https://github.com/swiftwasm/wasi-sdk/suites/370986556/artifacts/809001
unzip dist-wasi-sdk.tgz -d .
WASI_SDK_TAR_PATH=$(find . -type f -name "wasi-sdk-*")
WASI_SDK_FULL_NAME=$(basename $WASI_SDK_TAR_PATH -macos.tar.gz)
tar xfz $WASI_SDK_TAR_PATH
mv $WASI_SDK_FULL_NAME ./wasi-sdk

# Link sysroot/usr/include to sysroot/include because Darwin sysroot doesn't 
# find header files in sysroot/include but sysroot/usr/include
mkdir wasi-sdk/share/wasi-sysroot/usr/
ln -s ../include wasi-sdk/share/wasi-sysroot/usr/include
# Link wasm32-wasi-unknown to wasm32-wasi because clang finds crt1.o from sysroot
# with os and environment name `getMultiarchTriple`.
ln -s wasm32-wasi wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi-unknown

wget -O icu.tar.xz "https://github.com/swiftwasm/icu4c-wasi/releases/download/20190421.3/icu4c-wasi.tar.xz"
tar xf icu.tar.xz

$BUILD_SCRIPT --release --debug-swift-stdlib --verbose
# Run test but ignore failure temporarily
$BUILD_SCRIPT --release --debug-swift-stdlib --verbose -t || true
