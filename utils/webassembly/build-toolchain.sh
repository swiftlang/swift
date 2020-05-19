#/bin/bash

set -ex
SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"
UTILS_PATH="$(cd "$(dirname $0)" && pwd)"

WASI_SDK_PATH=$SOURCE_PATH/wasi-sdk

case $(uname -s) in
  Darwin)
    OS_SUFFIX=osx
    HOST_PRESET=webassembly-host
    TARGET_PRESET=webassembly-macos-target
  ;;
  Linux)
    OS_SUFFIX=linux
    HOST_PRESET=webassembly-linux-host
    TARGET_PRESET=webassembly-linux-target
  ;;
  *)
    echo "Unrecognised platform $(uname -s)"
    exit 1
  ;;
esac

YEAR=$(date +"%Y")
MONTH=$(date +"%m")
DAY=$(date +"%d")
TOOLCHAIN_VERSION="${YEAR}${MONTH}${DAY}"
TOOLCHAIN_NAME="swift-wasm-DEVELOPMENT-SNAPSHOT-${YEAR}-${MONTH}-${DAY}-a"
ARCHIVE="${TOOLCHAIN_NAME}-${OS_SUFFIX}.tar.gz"
INSTALLABLE_PACKAGE=$SOURCE_PATH/$ARCHIVE

PACKAGE_ARTIFACT="$SOURCE_PATH/swift-wasm-DEVELOPMENT-SNAPSHOT-${OS_SUFFIX}.tar.gz"

BUNDLE_IDENTIFIER="swiftwasm.${YEAR}${MONTH}${DAY}"
DISPLAY_NAME_SHORT="Swift for WebAssembly Development Snapshot"
DISPLAY_NAME="${DISPLAY_NAME_SHORT} ${YEAR}-${MONTH}-${DAY}"

HOST_TOOLCHAIN_DESTDIR=$SOURCE_PATH/host-toolchain-sdk
HOST_TOOLCHAIN_SDK=$HOST_TOOLCHAIN_DESTDIR/$TOOLCHAIN_NAME

# Avoid clang headers symlink issues
mkdir -p $HOST_TOOLCHAIN_SDK/usr/lib/clang/10.0.0

# Build the host toolchain and SDK first.
$SOURCE_PATH/swift/utils/build-script \
  --preset=$HOST_PRESET \
  INSTALL_DESTDIR="$HOST_TOOLCHAIN_DESTDIR" \
  TOOLCHAIN_NAME="$TOOLCHAIN_NAME" \
  C_CXX_LAUNCHER="$(which sccache)"

# Clean up the host toolchain build directory so that the next
# `build-script` invocation doesn't pick up wrong CMake config files.
# For some reason passing `--reconfigure` to `build-script` won't do this.
rm -rf $SOURCE_PATH/build/Ninja-ReleaseAssert/swift-*

# build the cross-compilled toolchain
$SOURCE_PATH/swift/utils/build-script \
  --preset=$TARGET_PRESET \
  INSTALL_DESTDIR="$SOURCE_PATH/install" \
  SOURCE_PATH="$SOURCE_PATH" \
  BUNDLE_IDENTIFIER="${BUNDLE_IDENTIFIER}" \
  DISPLAY_NAME="${DISPLAY_NAME}" \
  DISPLAY_NAME_SHORT="${DISPLAY_NAME_SHORT}" \
  TOOLCHAIN_NAME="${TOOLCHAIN_NAME}" \
  TOOLCHAIN_VERSION="${TOOLCHAIN_VERSION}" \
  C_CXX_LAUNCHER="$(which sccache)"

# Merge wasi-sdk and the toolchain
cp -a $WASI_SDK_PATH/lib/clang $HOST_TOOLCHAIN_SDK/usr/lib
cp -a $WASI_SDK_PATH/bin/{*ld,llvm-ar} $HOST_TOOLCHAIN_SDK/usr/bin
cp -r $WASI_SDK_PATH/share/wasi-sysroot $HOST_TOOLCHAIN_SDK/usr/share

# Replace absolute sysroot path with relative path
sed -i -e "s@\".*/include@\"../../../../share/wasi-sysroot/include@g" $SOURCE_PATH/install/$TOOLCHAIN_NAME/usr/lib/swift/wasi/wasm32/glibc.modulemap

# Copy the target environment stdlib into the toolchain

if [[ "$(uname)" == "Linux" ]]; then
  # Avoid copying usr/lib/swift/clang because our toolchain's one is a directory
  # but nightly's one is symbolic link. A simple copy fails to merge them.
  rsync -v -a $SOURCE_PATH/install/$TOOLCHAIN_NAME/usr/lib/ $HOST_TOOLCHAIN_SDK/usr/lib/ --exclude 'swift/clang'
else
  cp -v -a $SOURCE_PATH/install/$TOOLCHAIN_NAME/usr/lib/swift_static $HOST_TOOLCHAIN_SDK/usr/lib/swift_static
  cp -v -a $SOURCE_PATH/install/$TOOLCHAIN_NAME/usr/lib/swift/wasi $HOST_TOOLCHAIN_SDK/usr/lib/swift
fi

cd $HOST_TOOLCHAIN_DESTDIR
tar cfz $PACKAGE_ARTIFACT $TOOLCHAIN_NAME
