// RUN: not %target-swift-frontend -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/diags_from_header.h -serialize-diagnostics-path %t.dia 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-WARN
// RUN: test -s %t.dia
// RUN: c-index-test -read-diagnostics %t.dia 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-WARN

// Also check batch mode (multiple primary files).
// RUN: not %target-swift-frontend -typecheck -primary-file %s -primary-file %S/../Inputs/empty.swift -enable-objc-interop -import-objc-header %S/Inputs/diags_from_header.h -serialize-diagnostics-path %t.1.dia -serialize-diagnostics-path %t.2.dia 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-WARN
// RUN: test -s %t.1.dia
// RUN: c-index-test -read-diagnostics %t.1.dia 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-WARN
// RUN: test -s %t.2.dia
// RUN: c-index-test -read-diagnostics %t.2.dia 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-WARN

// Verify that -Wno-* options are applied.
// RUN: not %target-swift-frontend -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/diags_from_header.h -Xcc -Wno-#warnings 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-NO-WARN

// CHECK-WARN: diags_from_header.h:{{.*}}:2: warning: "here is some warning about something"
// CHECK-NO-WARN-NOT: warning about something
// CHECK: diags_from_header.h:{{.*}}:2: error: "but this one is an error"
