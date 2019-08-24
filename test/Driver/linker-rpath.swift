// REQUIRES: OS=macosx
// Note: This is really about the /host/ environment, but since there are RUN
// lines for multiple targets anyway it doesn't make a huge difference.

// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.14 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.14.3 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.14.4 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.15 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO-RPATH %s

// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-ios12 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-ios12.1 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-ios12.2 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO-RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-ios13 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO-RPATH %s

// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-tvos12 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-tvos12.1 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-tvos12.2 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO-RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target arm64-apple-tvos13 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO-RPATH %s

// RUN: %swiftc_driver_plain -driver-print-jobs -target armv7k-apple-watchos5 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target armv7k-apple-watchos5.1 %S/../Inputs/empty.swift | %FileCheck -check-prefix RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target armv7k-apple-watchos5.2 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO-RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target armv7k-apple-watchos6 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO-RPATH %s

// RPATH: bin/ld{{"? }}
// RPATH-SAME: -rpath {{"?/usr/lib/swift(-.+)?"? }}
// RPATH-SAME: -o {{[^ ]+}}

// NO-RPATH-NOT: -rpath{{ }}

// ### Test with -no-stdlib-rpath
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift -no-stdlib-rpath | %FileCheck -check-prefix NO-RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.15 %S/../Inputs/empty.swift -no-stdlib-rpath | %FileCheck -check-prefix NO-RPATH %s

// ### Test with -toolchain-stdlib-rpath
// RUN: %swiftc_driver_plain -driver-print-jobs -toolchain-stdlib-rpath -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift -resource-dir garbage/ | %FileCheck -check-prefix TOOLCHAIN-RPATH -DPLATFORM=%target-sdk-name %s
// RUN: %swiftc_driver_plain -driver-print-jobs -toolchain-stdlib-rpath -target x86_64-apple-macosx10.15 %S/../Inputs/empty.swift -resource-dir garbage/ | %FileCheck -check-prefix TOOLCHAIN-RPATH -DPLATFORM=%target-sdk-name %s

// TOOLCHAIN-RPATH: bin/ld{{"? }}
// TOOLCHAIN-RPATH-SAME: -rpath garbage/[[PLATFORM]]{{ }}
// TOOLCHAIN-RPATH-SAME: -o {{[^ ]+}}
