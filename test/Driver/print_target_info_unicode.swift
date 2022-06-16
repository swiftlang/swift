// RUN: %target-swift-frontend -print-target-info -target arm64-apple-ios12.0 -sdk /Applications🙉/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk | %FileCheck %s

// CHECK: "target": {
// CHECK-NEXT: "triple": "arm64-apple-ios12.0",
// CHECK: "sdkPath": "/Applications🙉/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk",
