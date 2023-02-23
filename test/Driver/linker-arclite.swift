// REQUIRES: OS=macosx
// Note: This is really about the /host/ environment, but since there are RUN
// lines for multiple targets anyway it doesn't make a huge difference.

// The libarclite library is no longer used for any Darwin platform, so this now just verifies that we never request it

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.11 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios9-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target arm64-apple-tvos9 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target armv7k-apple-watchos2 %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_ARCLITE %s

// NO_ARCLITE: bin/ld{{"? }}
// NO_ARCLITE-NOT: arclite
// NO_ARCLITE-NOT: CoreFoundation
// NO_ARCLITE: -o {{[^ ]+}}
