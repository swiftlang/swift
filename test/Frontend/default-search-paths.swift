// UNSUPPORTED: OS=windows-msvc

// Standard Apple paths.
// RUN: %swift_frontend_plain -target arm64-apple-macos15.4 %clang-importer-sdk-nosource -parse-stdlib -parse %s -Rmodule-loading 2>&1 | %FileCheck -check-prefix=APPLE %s
// APPLE: Implicit framework search paths:
// APPLE-NEXT: [0] SOURCE_DIR/test/Inputs/clang-importer-sdk/System/Library/Frameworks
// APPLE-NEXT: [1] SOURCE_DIR/test/Inputs/clang-importer-sdk/System/Library/SubFrameworks
// APPLE-NEXT: [2] SOURCE_DIR/test/Inputs/clang-importer-sdk/Library/Frameworks
// APPLE-NEXT: Runtime library import search paths:
// APPLE-NEXT: [0] BUILD_DIR/lib/swift/macosx
// APPLE-NEXT: [1] SOURCE_DIR/test/Inputs/clang-importer-sdk/usr/lib/swift
// APPLE-NEXT: (End of search path lists.)

// Non-Apple platforms don't have any implicit framework search paths.
// RUN: %swift_frontend_plain -target x86_64-unknown-linux-android %clang-importer-sdk-nosource -parse-stdlib -parse %s -Rmodule-loading 2>&1 | %FileCheck -check-prefix=ANDROID %s
// ANDROID: Implicit framework search paths:
// ANDROID-NEXT: Runtime library import search paths:
// ANDROID-NEXT: [0] BUILD_DIR/lib/swift/android
// ANDROID-NEXT: [1] BUILD_DIR/lib/swift/android/x86_64
// ANDROID-NEXT: [2] SOURCE_DIR/test/Inputs/clang-importer-sdk/usr/lib/swift/android
// ANDROID-NEXT: [3] SOURCE_DIR/test/Inputs/clang-importer-sdk/usr/lib/swift/android/x86_64
// ANDROID-NEXT: (End of search path lists.)

// -nostdimport doesn't set up any standard import paths at all.
// RUN: %swift_frontend_plain -target arm64-apple-macos15.4 %clang-importer-sdk-nosource -parse-stdlib -nostdimport -parse %s -Rmodule-loading 2>&1 | %FileCheck -check-prefix=NOSTDIMPORT %s
// RUN: %swift_frontend_plain -target x86_64-unknown-linux-android %clang-importer-sdk-nosource -parse-stdlib -nostdimport -parse %s -Rmodule-loading 2>&1 | %FileCheck -check-prefix=NOSTDIMPORT %s
// NOSTDIMPORT: Implicit framework search paths:
// NOSTDIMPORT-NEXT: Runtime library import search paths:
// NOSTDIMPORT-NEXT: (End of search path lists.)

// -nostdlibimport removes all of the standard imports from the SDK but leaves the toolchain ones.
// RUN: %swift_frontend_plain -target arm64-apple-macos15.4 %clang-importer-sdk-nosource -parse-stdlib -nostdlibimport -parse %s -Rmodule-loading 2>&1 | %FileCheck -check-prefix=APPLE-NOSTDLIBIMPORT %s
// APPLE-NOSTDLIBIMPORT: Implicit framework search paths:
// APPLE-NOSTDLIBIMPORT-NEXT: Runtime library import search paths:
// APPLE-NOSTDLIBIMPORT-NEXT: [0] BUILD_DIR/lib/swift/macosx
// APPLE-NOSTDLIBIMPORT-NEXT: (End of search path lists.)

// RUN: %swift_frontend_plain -target x86_64-unknown-linux-android %clang-importer-sdk-nosource -parse-stdlib -nostdlibimport -parse %s -Rmodule-loading 2>&1 | %FileCheck -check-prefix=ANDROID-NOSTDLIBIMPORT %s
// ANDROID-NOSTDLIBIMPORT: Implicit framework search paths:
// ANDROID-NOSTDLIBIMPORT-NEXT: Runtime library import search paths:
// ANDROID-NOSTDLIBIMPORT-NEXT: [0] BUILD_DIR/lib/swift/android
// ANDROID-NOSTDLIBIMPORT-NEXT: [1] BUILD_DIR/lib/swift/android/x86_64
// ANDROID-NOSTDLIBIMPORT-NEXT: (End of search path lists.)
