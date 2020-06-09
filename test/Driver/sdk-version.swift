// Check reading the SDKSettings.json from an SDK
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -sdk %S/Inputs/MacOSX10.15.versioned.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_10_15 %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -sdk %S/Inputs/MacOSX10.15.4.versioned.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_10_15_4 %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -sdk %S/Inputs/MacOSX10.15.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_UNVERSIONED %s

// MACOS_10_15: -target-sdk-version 10.15
// MACOS_10_15_4: -target-sdk-version 10.15.4
// MACOS_UNVERSIONED-NOT: -target-sdk-version
