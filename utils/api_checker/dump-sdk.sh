#!/bin/bash
set -e

DIR="$( cd "$( dirname "$0" )" && pwd )"
sh "$DIR/sdk-module-lists/create-module-lists.sh"

MacSDKPath=$(xcrun -sdk macosx -show-sdk-path)
IphoneOSSDKPath=$(xcrun -sdk iphoneos -show-sdk-path)
AppleTVOSSDKPath=$(xcrun -sdk appletvos -show-sdk-path)
WatchOSSDKPath=$(xcrun -sdk watchos -show-sdk-path)

XCTestMac="$MacSDKPath/../../Library/Frameworks/"
XCTestIphone="$IphoneOSSDKPath/../../Library/Frameworks/"
XCTestTV="$AppleTVOSSDKPath/../../Library/Frameworks/"

BASEDIR="$DIR/../../swift-sdk-digests/"

mkdir -p "${BASEDIR}"

DumpDir="/tmp/SDKDump"
rm -rf "/tmp/SDKDump"
mkdir "/tmp/SDKDump"

XCODE_VER=$(xcodebuild -version | sed '1d' | sed 's/Build version //g')

SWIFT_VER=${SWIFT_VER-3}
echo "SWIFT VERSION: ${SWIFT_VER}"

function sdk_info() {
  echo "Xcode Build: $XCODE_VER"
  echo "macOS SDK Build: $(xcodebuild -version -sdk macosx ProductBuildVersion)"
  echo "iOS SDK Build: $(xcodebuild -version -sdk iphoneos ProductBuildVersion)"
  echo "watchOS SDK Build: $(xcodebuild -version -sdk watchos ProductBuildVersion)"
  echo "tvOS SDK Build: $(xcodebuild -version -sdk appletvos ProductBuildVersion)"
}

sdk_info

XCODE_VER=-base-$SWIFT_VER

if [[ -z "$MODULE" ]]; then
  $SWIFT_API_DIGESTER -target x86_64-apple-macosx10.15 -o "$BASEDIR/macos$XCODE_VER.json" -dump-sdk -sdk "$MacSDKPath" -module-list-file "/tmp/modules-osx.txt" -F "$XCTestMac" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  $SWIFT_API_DIGESTER -target arm64-apple-ios13.5 -o "$BASEDIR/ios$XCODE_VER.json" -dump-sdk -sdk "$IphoneOSSDKPath" -module-list-file "/tmp/modules-iphoneos.txt" -F "$XCTestIphone" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  $SWIFT_API_DIGESTER -target arm64-apple-tvos13.4 -o "$BASEDIR/tvos$XCODE_VER.json" -dump-sdk -sdk "$AppleTVOSSDKPath" -module-list-file "/tmp/modules-tvos.txt" -F "$XCTestTV" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  $SWIFT_API_DIGESTER -target armv7k-apple-watchos6.2 -o "$BASEDIR/watchos$XCODE_VER.json" -dump-sdk -sdk "$WatchOSSDKPath" -module-list-file "/tmp/modules-watchos.txt" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  $SWIFT_API_DIGESTER -target x86_64-apple-macosx10.15 -o "$BASEDIR/macos-stdlib$XCODE_VER.json" -dump-sdk -sdk "$MacSDKPath" -module Swift -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
else
  ALL_MODULE_DIR="$BASEDIR/Xcode$XCODE_VER"
  rm -rf "$ALL_MODULE_DIR"
  mkdir "$ALL_MODULE_DIR"
  declare -a MODULE_NAMES=("Accelerate" "Foundation" "AppKit" "UIKit" "AudioToolbox" "Automator" "AVFoundation"
                           "AVKit" "CloudKit" "CoreBluetooth" "GameKit" "iAd" "Metal" "MetalKit"
                           "MetalPerformanceShaders" "OpenGLES" "Quartz" "XCTest" "UserNotifications" "CoreMedia" "VideoToolbox" "MediaToolbox" "MapKit")
  for MODULE in "${MODULE_NAMES[@]}"
  do
  echo "Generating baseline for framework ${MODULE}"
  MODULE_DIR="$ALL_MODULE_DIR/$MODULE"
  rm -rf "$MODULE_DIR"
  mkdir "$MODULE_DIR"
  $SWIFT_API_DIGESTER -target x86_64-apple-macosx10.15 -o "$MODULE_DIR/macos.json" -dump-sdk -sdk "$MacSDKPath" -module "$MODULE" -F "$XCTestMac" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  $SWIFT_API_DIGESTER -target arm64-apple-ios13.5 -o "$MODULE_DIR/ios.json" -dump-sdk -sdk "$IphoneOSSDKPath" -module "$MODULE" -F "$XCTestIphone" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  $SWIFT_API_DIGESTER -target arm64-apple-tvos13.4 -o "$MODULE_DIR/tvos.json" -dump-sdk -sdk "$AppleTVOSSDKPath" -module "$MODULE" -F "$XCTestTV" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  $SWIFT_API_DIGESTER -target armv7k-apple-watchos6.2 -o "$MODULE_DIR/watchos.json" -dump-sdk -sdk "$WatchOSSDKPath" -module "$MODULE" -module-cache-path "$DumpDir/ModuleCache" -swift-version $SWIFT_VER
  done
fi
