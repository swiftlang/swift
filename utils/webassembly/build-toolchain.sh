#!/bin/bash

set -ex
SOURCE_PATH="$(cd "$(dirname "$0")/../../.." && pwd)"
UTILS_PATH="$(cd "$(dirname "$0")" && pwd)"

WASI_SDK_PATH=$SOURCE_PATH/wasi-sdk

case $(uname -s) in
  Darwin)
    OS_SUFFIX=macos-x86_64
    HOST_PRESET=webassembly-host-install
    TARGET_PRESET=webassembly-macos-target-install
    HOST_SUFFIX=macosx-x86_64
  ;;
  Linux)
    if [ "$(grep RELEASE /etc/lsb-release)" == "DISTRIB_RELEASE=18.04" ]; then
      OS_SUFFIX=ubuntu18.04-x86_64
    elif [ "$(grep RELEASE /etc/lsb-release)" == "DISTRIB_RELEASE=20.04" ]; then
      OS_SUFFIX=ubuntu20.04-x86_64
    else
      echo "Unknown Ubuntu version"
      exit 1
    fi
    HOST_PRESET=webassembly-linux-host-install
    TARGET_PRESET=webassembly-linux-target-install
    HOST_SUFFIX=linux-x86_64
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

PACKAGE_ARTIFACT="$SOURCE_PATH/swift-wasm-DEVELOPMENT-SNAPSHOT-${OS_SUFFIX}.tar.gz"

BUNDLE_IDENTIFIER="swiftwasm.${YEAR}${MONTH}${DAY}"
DISPLAY_NAME_SHORT="Swift for WebAssembly Development Snapshot"
DISPLAY_NAME="${DISPLAY_NAME_SHORT} ${YEAR}-${MONTH}-${DAY}"

DIST_TOOLCHAIN_DESTDIR=$SOURCE_PATH/dist-toolchain-sdk
HOST_TOOLCHAIN_DESTDIR=$SOURCE_PATH/host-toolchain-sdk
TARGET_TOOLCHAIN_DESTDIR=$SOURCE_PATH/target-toolchain-sdk

DIST_TOOLCHAIN_SDK=$DIST_TOOLCHAIN_DESTDIR/$TOOLCHAIN_NAME
HOST_TOOLCHAIN_SDK=$HOST_TOOLCHAIN_DESTDIR/$TOOLCHAIN_NAME
TARGET_TOOLCHAIN_SDK=$TARGET_TOOLCHAIN_DESTDIR/$TOOLCHAIN_NAME


HOST_BUILD_ROOT=$SOURCE_PATH/host-build
TARGET_BUILD_ROOT=$SOURCE_PATH/target-build
HOST_BUILD_DIR=$HOST_BUILD_ROOT/Ninja-Release
TARGET_BUILD_DIR=$TARGET_BUILD_ROOT/Ninja-Release

build_host_toolchain() {
  # Build the host toolchain and SDK first.
  env SWIFT_BUILD_ROOT="$HOST_BUILD_ROOT" \
    "$SOURCE_PATH/swift/utils/build-script" \
    --preset-file="$UTILS_PATH/build-presets.ini" \
    --preset=$HOST_PRESET \
    --build-dir="$HOST_BUILD_DIR" \
    INSTALL_DESTDIR="$HOST_TOOLCHAIN_DESTDIR" \
    TOOLCHAIN_NAME="$TOOLCHAIN_NAME" \
    C_CXX_LAUNCHER="$(which sccache)"
}

build_target_toolchain() {
  mkdir -p "$HOST_BUILD_DIR/"
  # Copy the host build dir to reuse it.
  if [[ ! -e "$HOST_BUILD_DIR/llvm-$HOST_SUFFIX" ]]; then
    cp -r "$HOST_BUILD_DIR/llvm-$HOST_SUFFIX" "$TARGET_BUILD_DIR/llvm-$HOST_SUFFIX"
    # Clean up compiler-rt dir to cross compile it for host and wasm32
    (cd "$TARGET_BUILD_DIR/llvm-$HOST_SUFFIX" && ninja compiler-rt-clear)
  fi

  # build the cross-compilled toolchain
  env SWIFT_BUILD_ROOT="$TARGET_BUILD_ROOT" \
    "$SOURCE_PATH/swift/utils/build-script" \
    --preset-file="$UTILS_PATH/build-presets.ini" \
    --preset=$TARGET_PRESET \
    --build-dir="$TARGET_BUILD_DIR" \
    INSTALL_DESTDIR="$TARGET_TOOLCHAIN_DESTDIR" \
    SOURCE_PATH="$SOURCE_PATH" \
    BUNDLE_IDENTIFIER="${BUNDLE_IDENTIFIER}" \
    DISPLAY_NAME="${DISPLAY_NAME}" \
    DISPLAY_NAME_SHORT="${DISPLAY_NAME_SHORT}" \
    TOOLCHAIN_NAME="${TOOLCHAIN_NAME}" \
    TOOLCHAIN_VERSION="${TOOLCHAIN_VERSION}" \
    LLVM_BIN_DIR="${HOST_TOOLCHAIN_SDK}/usr/bin" \
    C_CXX_LAUNCHER="$(which sccache)"

  "$UTILS_PATH/build-foundation.sh" "$TARGET_TOOLCHAIN_SDK"
  "$UTILS_PATH/build-xctest.sh" "$TARGET_TOOLCHAIN_SDK"

}

merge_toolchains() {
  rm -rf "$DIST_TOOLCHAIN_DESTDIR"
  # Copy the base host toolchain
  cp -r "$HOST_TOOLCHAIN_DESTDIR" "$DIST_TOOLCHAIN_DESTDIR"

  # Merge wasi-sdk and the toolchain
  cp -r "$WASI_SDK_PATH/share/wasi-sysroot" "$DIST_TOOLCHAIN_SDK/usr/share"

  # Copy the target environment stdlib into the toolchain
  # Avoid copying usr/lib/swift/clang because our toolchain's one is a directory
  # but nightly's one is symbolic link. A simple copy fails to merge them.
  rsync -v -a "$TARGET_TOOLCHAIN_SDK/usr/lib/" "$DIST_TOOLCHAIN_SDK/usr/lib/" --exclude 'swift/clang'
  rsync -v -a "$TARGET_TOOLCHAIN_SDK/usr/bin/" "$DIST_TOOLCHAIN_SDK/usr/bin/"

  # Replace absolute sysroot path with relative path
  sed -i -e "s@\".*/include@\"../../../../share/wasi-sysroot/include@g" "$DIST_TOOLCHAIN_SDK/usr/lib/swift/wasi/wasm32/wasi.modulemap"
}

create_darwin_info_plist() {
  echo "-- Create Info.plist --"
  PLISTBUDDY_BIN="/usr/libexec/PlistBuddy"

  DARWIN_TOOLCHAIN_VERSION="5.3.${YEAR}${MONTH}${DAY}"
  BUNDLE_PREFIX="org.swiftwasm"
  DARWIN_TOOLCHAIN_BUNDLE_IDENTIFIER="${BUNDLE_PREFIX}.${YEAR}${MONTH}${DAY}"
  DARWIN_TOOLCHAIN_DISPLAY_NAME_SHORT="Swift for WebAssembly Snapshot"
  DARWIN_TOOLCHAIN_DISPLAY_NAME="${DARWIN_TOOLCHAIN_DISPLAY_NAME_SHORT} ${YEAR}-${MONTH}-${DAY}"
  DARWIN_TOOLCHAIN_ALIAS="swiftwasm"

  DARWIN_TOOLCHAIN_INFO_PLIST="${DIST_TOOLCHAIN_SDK}/usr/Info.plist"
  DARWIN_TOOLCHAIN_REPORT_URL="https://bugs.swift.org/"
  COMPATIBILITY_VERSION=2
  COMPATIBILITY_VERSION_DISPLAY_STRING="Xcode 8.0"
  DARWIN_TOOLCHAIN_CREATED_DATE="$(date -u +'%a %b %d %T GMT %Y')"
  SWIFT_USE_DEVELOPMENT_TOOLCHAIN_RUNTIME="YES"

  rm -f "${DARWIN_TOOLCHAIN_INFO_PLIST}"

  ${PLISTBUDDY_BIN} -c "Add DisplayName string '${DARWIN_TOOLCHAIN_DISPLAY_NAME}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add ShortDisplayName string '${DARWIN_TOOLCHAIN_DISPLAY_NAME_SHORT}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add CreatedDate date '${DARWIN_TOOLCHAIN_CREATED_DATE}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add CompatibilityVersion integer ${COMPATIBILITY_VERSION}" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add CompatibilityVersionDisplayString string ${COMPATIBILITY_VERSION_DISPLAY_STRING}" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add Version string '${DARWIN_TOOLCHAIN_VERSION}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add CFBundleIdentifier string '${DARWIN_TOOLCHAIN_BUNDLE_IDENTIFIER}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add ReportProblemURL string '${DARWIN_TOOLCHAIN_REPORT_URL}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add Aliases array" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add Aliases:0 string '${DARWIN_TOOLCHAIN_ALIAS}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add OverrideBuildSettings dict" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add OverrideBuildSettings:ENABLE_BITCODE string 'NO'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add OverrideBuildSettings:SWIFT_DISABLE_REQUIRED_ARCLITE string 'YES'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add OverrideBuildSettings:SWIFT_LINK_OBJC_RUNTIME string 'YES'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add OverrideBuildSettings:SWIFT_DEVELOPMENT_TOOLCHAIN string 'YES'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"
  ${PLISTBUDDY_BIN} -c "Add OverrideBuildSettings:SWIFT_USE_DEVELOPMENT_TOOLCHAIN_RUNTIME string '${SWIFT_USE_DEVELOPMENT_TOOLCHAIN_RUNTIME}'" "${DARWIN_TOOLCHAIN_INFO_PLIST}"

  chmod a+r "${DARWIN_TOOLCHAIN_INFO_PLIST}"
}

build_host_toolchain
build_target_toolchain

merge_toolchains

if [[ "$(uname)" == "Darwin" ]]; then
  create_darwin_info_plist
fi

cd "$DIST_TOOLCHAIN_DESTDIR"
tar cfz "$PACKAGE_ARTIFACT" "$TOOLCHAIN_NAME"
echo "Toolchain archive created successfully!"
