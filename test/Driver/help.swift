// Check that options printed with -help respect whether the driver is invoked
// as 'swift' or as 'swiftc'.

// RUN: %swiftc_driver -help | %FileCheck -check-prefix CHECK -check-prefix CHECK-SWIFTC %s
// RUN: %swiftc_driver -help | %FileCheck -check-prefix NEGATIVE -check-prefix NEGATIVE-SWIFTC %s

// RUN: %swift_driver -help | %FileCheck -check-prefix CHECK -check-prefix CHECK-SWIFT %s
// RUN: %swift_driver -help | %FileCheck -check-prefix NEGATIVE -check-prefix NEGATIVE-SWIFT %s

// Options that work with both 'swiftc' and 'swift':
// CHECK-DAG: -swift-version

// swiftc-only options:
// CHECK-SWIFTC-DAG: -typecheck
// NEGATIVE-SWIFT-NOT: -typecheck

// There are currently no interpreter-only options.

// Frontend options should not show up here.
// NEGATIVE-NOT: -merge-modules

// Options marked "help-hidden" should not show up here.
// NEGATIVE-NOT: -parse-stdlib
