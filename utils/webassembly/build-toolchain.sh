#/bin/bash

set -ex
SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"
UTILS_PATH="$(cd "$(dirname $0)" && pwd)"

WASI_SDK_PATH=$SOURCE_PATH/wasi-sdk

case $(uname -s) in
  Darwin)
    OS_SUFFIX=osx
    BUILD_SCRIPT=$UTILS_PATH/build-mac.sh
  ;;
  Linux)
    OS_SUFFIX=linux
    BUILD_SCRIPT=$UTILS_PATH/build-linux.sh
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

$BUILD_SCRIPT \
  --install_destdir="$SOURCE_PATH/install" \
  --installable_package="$INSTALLABLE_PACKAGE" \
  --install-prefix=/$TOOLCHAIN_NAME/usr \
  --swift-install-components "autolink-driver;compiler;clang-builtin-headers;stdlib;sdk-overlay;parser-lib;editor-integration;tools;testsuite-tools;toolchain-tools;license;sourcekit-inproc;swift-remote-mirror;swift-remote-mirror-headers;clang-resource-dir-symlink"
  --llvm-install-components "clang" \
  --install-swift \
  --llvm-install-components "clang" \
  --darwin-toolchain-bundle-identifier="${BUNDLE_IDENTIFIER}" \
  --darwin-toolchain-display-name="${DISPLAY_NAME}" \
  --darwin-toolchain-display-name-short="${DISPLAY_NAME_SHORT}" \
  --darwin-toolchain-name="${TOOLCHAIN_NAME}" \
  --darwin-toolchain-version="${TOOLCHAIN_VERSION}" \
  --darwin-toolchain-alias="swift" \
  "$@"


NIGHTLY_TOOLCHAIN=$SOURCE_PATH/swift-nightly-toolchain
if [ ! -e $NIGHTLY_TOOLCHAIN ]; then
  $UTILS_PATH/install-nightly-toolchain.sh
fi

TMP_DIR=$(mktemp -d)
cd $TMP_DIR
tar xfz $INSTALLABLE_PACKAGE $TOOLCHAIN_NAME
cd $TMP_DIR/$TOOLCHAIN_NAME

# Merge wasi-sdk and toolchain
cp -r $WASI_SDK_PATH/lib/clang usr/lib
cp -a $WASI_SDK_PATH/bin/*ld usr/bin
cp -r $WASI_SDK_PATH/share/wasi-sysroot usr/share

# Build SwiftPM and install it into toolchain
$UTILS_PATH/build-swiftpm.sh $TMP_DIR/$TOOLCHAIN_NAME

# Replace absolute sysroot path with relative path
sed -i -e "s@\".*/include@\"../../../../share/wasi-sysroot/include@g" $TMP_DIR/$TOOLCHAIN_NAME/usr/lib/swift/wasi/wasm32/glibc.modulemap

# Copy nightly-toolchain's host environment stdlib into toolchain

if [[ "$(uname)" == "Linux" ]]; then
  cp -a $NIGHTLY_TOOLCHAIN/usr/lib/* $TMP_DIR/$TOOLCHAIN_NAME/usr/lib
else
  cp -r $NIGHTLY_TOOLCHAIN/usr/lib/swift/macosx $TMP_DIR/$TOOLCHAIN_NAME/usr/lib/swift
fi

cd $TMP_DIR
tar cfz $PACKAGE_ARTIFACT $TOOLCHAIN_NAME
