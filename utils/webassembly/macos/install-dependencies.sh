#/bin/bash

set -ex

brew uninstall python@2 || true
sudo pip install six
brew install cmake ninja llvm sccache wasmer

SOURCE_PATH="$( cd "$(dirname $0)/../../../../" && pwd  )"
SWIFT_PATH=$SOURCE_PATH/swift
cd $SWIFT_PATH

./utils/update-checkout --clone --scheme wasm --skip-repository swift

cd $SOURCE_PATH

$SWIFT_PATH/utils/webassembly/install-wasi-sdk.sh macos macos-10.15

# Link sysroot/usr/include to sysroot/include because Darwin sysroot doesn't 
# find header files in sysroot/include but sysroot/usr/include
mkdir wasi-sdk/share/wasi-sysroot/usr/
ln -s ../include wasi-sdk/share/wasi-sysroot/usr/include
# Link wasm32-wasi-unknown to wasm32-wasi because clang finds crt1.o from sysroot
# with os and environment name `getMultiarchTriple`.
ln -s wasm32-wasi wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi-unknown
