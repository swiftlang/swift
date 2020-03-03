#/bin/bash

set -x
SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"
UTILS_PATH="$(cd "$(dirname $0)" && pwd)"

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
TOOLCHAIN_VERSION="5.2.${YEAR}${MONTH}${DAY}"
TOOLCHAIN_NAME="swift-wasm-5.2-DEVELOPMENT-SNAPSHOT-${YEAR}-${MONTH}-${DAY}-a"
ARCHIVE="${TOOLCHAIN_NAME}-${OS_SUFFIX}.tar.gz"

BUNDLE_IDENTIFIER="${BUNDLE_PREFIX}.${YEAR}${MONTH}${DAY}"
DISPLAY_NAME_SHORT="Swift for WebAssembly Development Snapshot"
DISPLAY_NAME="${DISPLAY_NAME_SHORT} ${YEAR}-${MONTH}-${DAY}"

$BUILD_SCRIPT \
  --install_destdir="$SOURCE_PATH/install" \
  --installable_package="$SOURCE_PATH/$ARCHIVE" \
  --install-prefix=/$TOOLCHAIN_NAME/usr \
  --install-swift \
  --darwin-toolchain-bundle-identifier="${BUNDLE_IDENTIFIER}" \
  --darwin-toolchain-display-name="${DISPLAY_NAME}" \
  --darwin-toolchain-display-name-short="${DISPLAY_NAME_SHORT}" \
  --darwin-toolchain-name="${TOOLCHAIN_NAME}" \
  --darwin-toolchain-version="${TOOLCHAIN_VERSION}" \
  "$@"
