#/bin/bash

set -ex

brew uninstall python@2 || true
brew install cmake ninja llvm sccache wasmer

SOURCE_PATH="$( cd "$(dirname $0)/../../../../" && pwd  )"
SWIFT_PATH=$SOURCE_PATH/swift
cd $SWIFT_PATH

./utils/update-checkout --clone --scheme wasm --skip-repository swift

cd $SOURCE_PATH

wget -O dist-wasi-sdk.tgz.zip "https://github.com/swiftwasm/wasi-sdk/releases/download/0.2.0-swiftwasm/dist-macos-latest.tgz.zip"
unzip dist-wasi-sdk.tgz.zip -d .
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

wget -O icu.tar.xz "https://github.com/swiftwasm/icu4c-wasi/releases/download/0.5.0/icu4c-wasi.tar.xz"
tar xf icu.tar.xz
