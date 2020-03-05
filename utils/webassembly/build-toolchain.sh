#/bin/bash

set -x
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

BUNDLE_IDENTIFIER="${BUNDLE_PREFIX}.${YEAR}${MONTH}${DAY}"
DISPLAY_NAME_SHORT="Swift for WebAssembly Development Snapshot"
DISPLAY_NAME="${DISPLAY_NAME_SHORT} ${YEAR}-${MONTH}-${DAY}"

$BUILD_SCRIPT \
  --install_destdir="$SOURCE_PATH/install" \
  --installable_package="$INSTALLABLE_PACKAGE" \
  --install-prefix=/$TOOLCHAIN_NAME/usr \
  --install-swift \
  --darwin-toolchain-bundle-identifier="${BUNDLE_IDENTIFIER}" \
  --darwin-toolchain-display-name="${DISPLAY_NAME}" \
  --darwin-toolchain-display-name-short="${DISPLAY_NAME_SHORT}" \
  --darwin-toolchain-name="${TOOLCHAIN_NAME}" \
  --darwin-toolchain-version="${TOOLCHAIN_VERSION}" \
  --darwin-toolchain-alias="swift" \
  "$@"

TMP_DIR=$(mktemp -d)
cd $TMP_DIR
tar xfz $INSTALLABLE_PACKAGE $TOOLCHAIN_NAME
cd $TMP_DIR/$TOOLCHAIN_NAME

# Merge wasi-sdk and toolchain
cp -r $WASI_SDK_PATH/lib/clang usr/lib
cp $WASI_SDK_PATH/bin/* usr/bin
cp -r $WASI_SDK_PATH/share/wasi-sysroot usr/share

cd $TMP_DIR
tar cfz $PACKAGE_ARTIFACT $TOOLCHAIN_NAME
