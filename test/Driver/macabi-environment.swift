// Tests to check that the driver finds standard library in the macabi environment.

// UNSUPPORTED: OS=windows-msvc

// RUN: %swiftc_driver -sdk "" -sdk "" -driver-print-jobs -target x86_64-apple-ios13.1-macabi -sdk %S/../Inputs/clang-importer-sdk %s | %FileCheck -check-prefix=IOS13-MACABI %s
// IOS13-MACABI: bin/swift
// IOS13-MACABI: -target x86_64-apple-ios13.1-macabi

// IOS13-MACABI: bin/ld
// IOS13-MACABI-DAG: -L [[MACCATALYST_STDLIB_PATH:[^ ]+/lib/swift/maccatalyst]]
// IOS13-MACABI-DAG: -L [[MACOSX_STDLIB_PATH:[^ ]+/lib/swift/macosx]]
// IOS13-MACABI-DAG: -L [[MACCATALYST_SDK_STDLIB_PATH:[^ ]+/clang-importer-sdk/System/iOSSupport/usr/lib/swift]]
// IOS13-MACABI-DAG: -L [[MACOSX_SDK_STDLIB_PATH:[^ ]+/clang-importer-sdk/usr/lib/swift]]
// IOS13-MACABI-DAG: -rpath [[MACCATALYST_STDLIB_PATH]]
// IOS13-MACABI-DAG: -rpath [[MACOSX_STDLIB_PATH]]
// IOS13-MACABI-DAG: -rpath [[MACCATALYST_SDK_STDLIB_PATH]]
// IOS13-MACABI-DAG: -rpath [[MACOSX_SDK_STDLIB_PATH]]
// IOS13-MACABI-DAG: -platform_version mac-catalyst 13.1.0 0.0.0

// Adjust iOS versions < 13.1 to 13.1 for the linker's sake.

// RUN: %swiftc_driver -sdk "" -sdk "" -driver-print-jobs -target x86_64-apple-ios12.0-macabi -sdk %S/../Inputs/clang-importer-sdk %s | %FileCheck -check-prefix=IOS12-MACABI %s
// IOS12-MACABI: bin/swift
// IOS12-MACABI: -target x86_64-apple-ios12.0-macabi

// IOS12-MACABI: bin/ld
// IOS12-MACABI-DAG: -L [[MACCATALYST_STDLIB_PATH:[^ ]+/lib/swift/maccatalyst]]
// IOS12-MACABI-DAG: -L [[MACOSX_STDLIB_PATH:[^ ]+/lib/swift/macosx]]
// IOS12-MACABI-DAG: -L [[MACCATALYST_SDK_STDLIB_PATH:[^ ]+/clang-importer-sdk/System/iOSSupport/usr/lib/swift]]
// IOS12-MACABI-DAG: -L [[MACOSX_SDK_STDLIB_PATH:[^ ]+/clang-importer-sdk/usr/lib/swift]]
// IOS12-MACABI-DAG: -rpath [[MACCATALYST_STDLIB_PATH]]
// IOS12-MACABI-DAG: -rpath [[MACOSX_STDLIB_PATH]]
// IOS12-MACABI-DAG: -rpath [[MACCATALYST_SDK_STDLIB_PATH]]
// IOS12-MACABI-DAG: -rpath [[MACOSX_SDK_STDLIB_PATH]]
// IOS12-MACABI-DAG: -platform_version mac-catalyst 13.1.0 0.0.0

// RUN: %swiftc_driver -driver-print-jobs -target arm64-apple-ios12.0-macabi -sdk %S/../Inputs/clang-importer-sdk %s | %FileCheck -check-prefix=IOS14-MACABI %s
// IOS14-MACABI: -platform_version mac-catalyst 14.0.0 0.0.0

// Test using target-variant to build zippered outputs

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -c -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi %s | %FileCheck -check-prefix=ZIPPERED-VARIANT-OBJECT %s
// ZIPPERED-VARIANT-OBJECT: bin/swift
// ZIPPERED-VARIANT-OBJECT: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi

// RUN: %swiftc_driver -sdk "" -sdk "" -driver-print-jobs -emit-library -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi -module-name foo %s | %FileCheck -check-prefix=ZIPPERED-VARIANT-LIBRARY %s
// ZIPPERED-VARIANT-LIBRARY: bin/swift
// ZIPPERED-VARIANT-LIBRARY: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi

// ZIPPERED-VARIANT-LIBRARY: bin/ld
// ZIPPERED-VARIANT-LIBRARY: -platform_version macos 10.14.0 0.0.0 -platform_version mac-catalyst 13.1.0 0.0.0

// Make sure we pass the -target-variant when creating the pre-compiled header.
// RUN: %swiftc_driver -sdk "" -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi -enable-bridging-pch -import-objc-header %S/Inputs/bridging-header.h %s | %FileCheck -check-prefix=ZIPPERED-VARIANT-PCH %s
// ZIPPERED-VARIANT-PCH: bin/swift
// ZIPPERED-VARIANT-PCH: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi
// ZIPPERED_VARIANT-PCH:  -emit-pch
// ZIPPERED-VARIANT-PCH: bin/swift
// ZIPPERED-VARIANT-PCH: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi
// ZIPPERED-VARIANT-PCH: bin/ld
// ZIPPERED-VARIANT-PCH: -platform_version macos 10.14.0 0.0.0 -platform_version mac-catalyst 13.1.0 0.0.0

// Test using 'reverse' target-variant to build zippered outputs when the primary
// target is ios-macabi

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -c -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.14 %s | %FileCheck -check-prefix=REVERSE-ZIPPERED-VARIANT-OBJECT %s
// REVERSE-ZIPPERED-VARIANT-OBJECT: bin/swift
// REVERSE-ZIPPERED-VARIANT-OBJECT: -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.14

// RUN: %swiftc_driver -sdk "" -sdk "" -driver-print-jobs -emit-library -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.14 -module-name foo %s | %FileCheck -check-prefix=REVERSE-ZIPPERED-VARIANT-LIBRARY %s
// REVERSE-ZIPPERED-VARIANT-LIBRARY: bin/swift
// REVERSE-ZIPPERED-VARIANT-LIBRARY: -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.14

// REVERSE-ZIPPERED-VARIANT-LIBRARY: bin/ld
// REVERSE-ZIPPERED-VARIANT-LIBRARY: -platform_version mac-catalyst 13.1.0 0.0.0 -platform_version macos 10.14.0

// Make sure we pass the -target-variant when creating the pre-compiled header.
// RUN: %swiftc_driver -sdk "" -sdk "" -driver-print-jobs -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.14 -enable-bridging-pch -import-objc-header %S/Inputs/bridging-header.h %s | %FileCheck -check-prefix=REVERSE-ZIPPERED-VARIANT-PCH %s
// REVERSE-ZIPPERED-VARIANT-PCH: bin/swift
// REVERSE-ZIPPERED-VARIANT-PCH: -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.14
// REVERSE-ZIPPERED_VARIANT-PCH:  -emit-pch
// REVERSE-ZIPPERED-VARIANT-PCH: bin/swift
// REVERSE-ZIPPERED-VARIANT-PCH: -target x86_64-apple-ios13.1-macabi -target-variant x86_64-apple-macosx10.14
// REVERSE-ZIPPERED-VARIANT-PCH: bin/ld
// REVERSE-ZIPPERED-VARIANT-PCH: -platform_version mac-catalyst 13.1.0 0.0.0 -platform_version macos 10.14.0 0.0.0

// RUN: not %swiftc_driver -sdk "" -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0 %s 2>&1 | %FileCheck --check-prefix=UNSUPPORTED-TARGET-VARIANT %s
// RUN: not %swiftc_driver -sdk "" -target x86_64-apple-ios13.0 -target-variant x86_64-apple-macosx10.14 %s 2>&1 | %FileCheck --check-prefix=UNSUPPORTED-TARGET %s

// UNSUPPORTED-TARGET-VARIANT: error: unsupported '-target-variant' value {{.*}}; use 'ios-macabi' instead
// UNSUPPORTED-TARGET: error: unsupported '-target' value {{.*}}; use 'ios-macabi' instead

// When compiling for iOS, pass iphoneos_version_min to the linker, not maccatalyst_version_min.

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target arm64-apple-ios13.0 -sdk %S/../Inputs/clang-importer-sdk %s | %FileCheck -check-prefix=IOS13-NO-MACABI -implicit-check-not=mac-catalyst %s
// IOS13-NO-MACABI: bin/swift
// IOS13-NO-MACABI: -target arm64-apple-ios13.0

// IOS13-NO-MACABI: bin/ld
// IOS13-NO-MACABI-DAG: -L {{[^ ]+/lib/swift/iphoneos}}
// IOS13-NO-MACABI-DAG: -L {{[^ ]+/clang-importer-sdk/usr/lib/swift}}
// IOS13-NO-MACABI-DAG: -platform_version ios 13.0.0

// Check reading the SDKSettings.json from an SDK and using it to map Catalyst
// SDK version information.

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi -sdk %S/Inputs/MacOSX10.15.versioned.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_10_15_ZIPPERED %s
// MACOS_10_15_ZIPPERED: -target-sdk-version 10.15
// MACOS_10_15_ZIPPERED: -target-variant-sdk-version 13.1
// MACOS_10_15_ZIPPERED: -platform_version macos 10.14.0 10.15.0
// MACOS_10_15_ZIPPERED: -platform_version mac-catalyst 13.1.0 13.1.0

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.1-macabi -sdk %S/Inputs/MacOSX10.15.4.versioned.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_10_15_4_ZIPPERED %s
// MACOS_10_15_4_ZIPPERED: -target-sdk-version 10.15.4
// MACOS_10_15_4_ZIPPERED: -target-variant-sdk-version 13.4
// MACOS_10_15_4_ZIPPERED: -platform_version macos 10.14.0 10.15.4
// MACOS_10_15_4_ZIPPERED: -platform_version mac-catalyst 13.1.0 13.4.0

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target-variant x86_64-apple-macosx10.14 -target x86_64-apple-ios13.1-macabi -sdk %S/Inputs/MacOSX10.15.4.versioned.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_10_15_4_REVERSE_ZIPPERED %s
// MACOS_10_15_4_REVERSE_ZIPPERED: -target-sdk-version 13.4
// MACOS_10_15_4_REVERSE_ZIPPERED: -target-variant-sdk-version 10.15.4
// MACOS_10_15_4_REVERSE_ZIPPERED: -platform_version mac-catalyst 13.1.0 13.4.0
// MACOS_10_15_4_REVERSE_ZIPPERED: -platform_version macos 10.14.0 10.15.4

