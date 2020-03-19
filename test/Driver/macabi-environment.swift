// Tests to check that the driver finds standard library in the macabi environment.

// UNSUPPORTED: windows

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios13.0-macabi -sdk %S/../Inputs/clang-importer-sdk %s | %FileCheck -check-prefix=IOS13-MACABI %s
// IOS13-MACABI: bin/swift
// IOS13-MACABI: -target x86_64-apple-ios13.0-macabi

// IOS13-MACABI: bin/ld
// IOS13-MACABI-DAG: -L [[MACCATALYST_STDLIB_PATH:[^ ]+/lib/swift/maccatalyst]]
// IOS13-MACABI-DAG: -L [[MACOSX_STDLIB_PATH:[^ ]+/lib/swift/macosx]]
// IOS13-MACABI-DAG: -L [[MACCATALYST_SDK_STDLIB_PATH:[^ ]+/clang-importer-sdk/System/iOSSupport/usr/lib/swift]]
// IOS13-MACABI-DAG: -L [[MACOSX_SDK_STDLIB_PATH:[^ ]+/clang-importer-sdk/usr/lib/swift]]
// IOS13-MACABI-DAG: -rpath [[MACCATALYST_STDLIB_PATH]]
// IOS13-MACABI-DAG: -rpath [[MACOSX_STDLIB_PATH]]
// IOS13-MACABI-DAG: -rpath [[MACCATALYST_SDK_STDLIB_PATH]]
// IOS13-MACABI-DAG: -rpath [[MACOSX_SDK_STDLIB_PATH]]
// IOS13-MACABI-DAG: -maccatalyst_version_min 13.0.0


// Test using target-variant to build zippered outputs

// RUN: %swiftc_driver -driver-print-jobs -c -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0-macabi %s | %FileCheck -check-prefix=ZIPPERED-VARIANT-OBJECT %s
// ZIPPERED-VARIANT-OBJECT: bin/swift
// ZIPPERED-VARIANT-OBJECT: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0-macabi

// RUN: %swiftc_driver -driver-print-jobs -emit-library -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0-macabi -module-name foo %s | %FileCheck -check-prefix=ZIPPERED-VARIANT-LIBRARY %s
// ZIPPERED-VARIANT-LIBRARY: bin/swift
// ZIPPERED-VARIANT-LIBRARY: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0-macabi

// ZIPPERED-VARIANT-LIBRARY: bin/ld
// ZIPPERED-VARIANT-LIBRARY: -macosx_version_min 10.14.0 -maccatalyst_version_min 13.0.0

// Make sure we pass the -target-variant when creating the pre-compiled header.
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0-macabi -enable-bridging-pch -import-objc-header %S/Inputs/bridging-header.h %s | %FileCheck -check-prefix=ZIPPERED-VARIANT-PCH %s
// ZIPPERED-VARIANT-PCH: bin/swift
// ZIPPERED-VARIANT-PCH: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0-macabi
// ZIPPERED_VARIANT-PCH  -emit-pch
// ZIPPERED-VARIANT-PCH: bin/swift
// ZIPPERED-VARIANT-PCH: -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0-macabi
// ZIPPERED-VARIANT-PCH: bin/ld
// ZIPPERED-VARIANT-PCH: -macosx_version_min 10.14.0 -maccatalyst_version_min 13.0.0

// Test using 'reverse' target-variant to build zippered outputs when the primary
// target is ios-macabi

// RUN: %swiftc_driver -driver-print-jobs -c -target x86_64-apple-ios13.0-macabi -target-variant x86_64-apple-macosx10.14 %s | %FileCheck -check-prefix=REVERSE-ZIPPERED-VARIANT-OBJECT %s
// REVERSE-ZIPPERED-VARIANT-OBJECT: bin/swift
// REVERSE-ZIPPERED-VARIANT-OBJECT: -target x86_64-apple-ios13.0-macabi -target-variant x86_64-apple-macosx10.14

// RUN: %swiftc_driver -driver-print-jobs -emit-library -target x86_64-apple-ios13.0-macabi -target-variant x86_64-apple-macosx10.14 -module-name foo %s | %FileCheck -check-prefix=REVERSE-ZIPPERED-VARIANT-LIBRARY %s
// REVERSE-ZIPPERED-VARIANT-LIBRARY: bin/swift
// REVERSE-ZIPPERED-VARIANT-LIBRARY: -target x86_64-apple-ios13.0-macabi -target-variant x86_64-apple-macosx10.14

// REVERSE-ZIPPERED-VARIANT-LIBRARY: bin/ld
// REVERSE-ZIPPERED-VARIANT-LIBRARY: -maccatalyst_version_min 13.0.0 -macosx_version_min 10.14.0

// Make sure we pass the -target-variant when creating the pre-compiled header.
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios13.0-macabi -target-variant x86_64-apple-macosx10.14 -enable-bridging-pch -import-objc-header %S/Inputs/bridging-header.h %s | %FileCheck -check-prefix=REVERSE-ZIPPERED-VARIANT-PCH %s
// REVERSE-ZIPPERED-VARIANT-PCH: bin/swift
// REVERSE-ZIPPERED-VARIANT-PCH: -target x86_64-apple-ios13.0-macabi -target-variant x86_64-apple-macosx10.14
// REVERSE-ZIPPERED_VARIANT-PCH  -emit-pch
// REVERSE-ZIPPERED-VARIANT-PCH: bin/swift
// REVERSE-ZIPPERED-VARIANT-PCH: -target x86_64-apple-ios13.0-macabi -target-variant x86_64-apple-macosx10.14
// REVERSE-ZIPPERED-VARIANT-PCH: bin/ld
// REVERSE-ZIPPERED-VARIANT-PCH: -maccatalyst_version_min 13.0.0 -macosx_version_min 10.14.0

// RUN: not %swiftc_driver -target x86_64-apple-macosx10.14 -target-variant x86_64-apple-ios13.0 %s 2>&1 | %FileCheck --check-prefix=UNSUPPORTED-TARGET-VARIANT %s
// RUN: not %swiftc_driver -target x86_64-apple-ios13.0 -target-variant x86_64-apple-macosx10.14 %s 2>&1 | %FileCheck --check-prefix=UNSUPPORTED-TARGET %s

// UNSUPPORTED-TARGET-VARIANT: error: unsupported '-target-variant' value {{.*}}; use 'ios-macabi' instead
// UNSUPPORTED-TARGET: error: unsupported '-target' value {{.*}}; use 'ios-macabi' instead

// When compiling for iOS, pass iphoneos_version_min to the linker, not maccatalyst_version_min.

// RUN: %swiftc_driver -driver-print-jobs -target arm64-apple-ios13.0 -sdk %S/../Inputs/clang-importer-sdk %s | %FileCheck -check-prefix=IOS13-NO-MACABI -implicit-check-not=maccatalyst_version_min %s
// IOS13-NO-MACABI: bin/swift
// IOS13-NO-MACABI: -target arm64-apple-ios13.0

// IOS13-NO-MACABI: bin/ld
// IOS13-NO-MACABI-DAG: -L {{[^ ]+/lib/swift/iphoneos}}
// IOS13-NO-MACABI-DAG: -L {{[^ ]+/clang-importer-sdk/usr/lib/swift}}
// IOS13-NO-MACABI-DAG: -iphoneos_version_min 13.0.0
