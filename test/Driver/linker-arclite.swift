// REQUIRES: OS=macosx
// Note: This is really about the /host/ environment, but since there are RUN
// lines for multiple targets anyway it doesn't make a huge difference.

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift | %FileCheck %s

// CHECK: bin/ld{{"? }}
// CHECK-SAME: -force_load {{[^ ]+/lib/arc/libarclite_macosx.a}} -framework CoreFoundation
// CHECK-SAME: -o {{[^ ]+}}


// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios8.0 %S/../Inputs/empty.swift | %FileCheck -check-prefix IOS_ARCLITE %s

// IOS_ARCLITE: bin/ld{{"? }}
// IOS_ARCLITE: -force_load {{[^ ]+/lib/arc/libarclite_iphonesimulator.a}}
// IOS_ARCLITE: -o {{[^ ]+}}


// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.11 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 %S/../Inputs/empty.swift | %FileCheck -check-prefix ANY_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios9 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios8 %S/../Inputs/empty.swift | %FileCheck -check-prefix ANY_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target arm64-apple-tvos9 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target armv7k-apple-watchos2 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s

// NO_ARCLITE: bin/ld{{"? }}
// NO_ARCLITE-NOT: arclite
// NO_ARCLITE: -o {{[^ ]+}}

// ANY_ARCLITE: bin/ld{{"? }}
// ANY_ARCLITE: -force_load {{[^ ]+}}/lib/arc/libarclite_{{.+}}.a
// ANY_ARCLITE: -o {{[^ ]+}}
