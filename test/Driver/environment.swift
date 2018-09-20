// UNSUPPORTED: objc_interop
// Apple's "System Integrity Protection" makes this test fail on OS X.

// RUN: %swift_driver -target x86_64-unknown-gnu-linux -L/foo/ -driver-use-frontend-path %S/Inputs/print-var.sh %s LD_LIBRARY_PATH | %FileCheck -check-prefix=CHECK${LD_LIBRARY_PATH+_LAX} %s

// CHECK: {{^/foo/:[^:]+/lib/swift/linux/x86_64$}}
// CHECK_LAX: {{^/foo/:[^:]+/lib/swift/linux/x86_64}}
