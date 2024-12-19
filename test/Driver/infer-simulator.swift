// Test the inference of the "simulator" environment within the driver.

// RUN: %swiftc_driver -sdk '""' -driver-print-jobs -target x86_64-apple-ios11.0 %s > %t.simple.txt 2>&1
// RUN: %FileCheck -check-prefix WARN_IOS_SIMULATOR %s < %t.simple.txt
// WARN_IOS_SIMULATOR:  warning: inferring simulator environment for target 'x86_64-apple-ios11.0'; use '-target x86_64-apple-ios11.0-simulator' instead

// RUN: %swiftc_driver -sdk '""' -driver-print-jobs -target x86_64-apple-tvos10.0 %s > %t.simple.txt 2>&1
// RUN: %FileCheck -check-prefix WARN_TVOS_SIMULATOR %s < %t.simple.txt
// WARN_TVOS_SIMULATOR: inferring simulator environment for target 'x86_64-apple-tvos10.0'; use '-target x86_64-apple-tvos10.0-simulator' instead

// RUN: %swiftc_driver -sdk '""' -driver-print-jobs -target i386-apple-watchos4.0 %s > %t.simple.txt 2>&1
// RUN: %FileCheck -check-prefix WARN_WATCHOS_SIMULATOR %s < %t.simple.txt
// WARN_WATCHOS_SIMULATOR: warning: inferring simulator environment for target 'i386-apple-watchos4.0'; use '-target i386-apple-watchos4.0-simulator' instead
