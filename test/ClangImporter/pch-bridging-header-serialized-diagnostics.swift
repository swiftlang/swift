// RUN: rm -f %t.*

// Check that when a driver is run with -serialize-diagnostics, it picks a .dia location and passes it to the frontend.
// RUN: %target-swiftc_driver -typecheck %s -enable-bridging-pch -serialize-diagnostics -import-objc-header %S/Inputs/bad-bridging-header.h -driver-print-jobs 2>&1 -diagnostic-style llvm | %FileCheck -check-prefix=CHECK-ARG %s
// CHECK-ARG: -serialize-diagnostics-path

// Check that when a frontend is run in -emit-pch mode with -serialize-diagnostics-path, the diagnostics -- both textual and serialized -- reflect the error.
// RUN: not %target-swift-frontend -emit-pch %S/Inputs/bad-bridging-header.h -o %t.pch -serialize-diagnostics -serialize-diagnostics-path %t.dia -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=CHECK-DIAG %s
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DIAG -input-file=%t.deserialized_diagnostics.txt %s

// RUN: not %target-swift-frontend -emit-pch %S/Inputs/bad-bridging-header.h -pch-output-dir %t/pch -serialize-diagnostics -serialize-diagnostics-path %t.dia2 -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=CHECK-DIAG %s
// RUN: c-index-test -read-diagnostics %t.dia2 > %t.deserialized_diagnostics.txt2 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DIAG -input-file=%t.deserialized_diagnostics.txt2 %s

// RUN: not %target-swift-frontend -import-objc-header %S/Inputs/bad-bridging-header.h -pch-output-dir %t/pch -typecheck %s -serialize-diagnostics -serialize-diagnostics-path %t.dia3 -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=CHECK-DIAG %s
// RUN: c-index-test -read-diagnostics %t.dia3 > %t.deserialized_diagnostics.txt3 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DIAG -input-file=%t.deserialized_diagnostics.txt3 %s

// CHECK-DIAG: bad-bridging-header.h:1:10: error: 'this-header-does-not-exist.h' file not found
// CHECK-DIAG: failed to emit precompiled header
