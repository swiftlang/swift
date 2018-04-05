// RUN: not %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/diags_from_header.h 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-WARN

// RUN: not %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/diags_from_header.h -Xcc -Wno-#warnings 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-NO-WARN

// CHECK-WARN: diags_from_header.h:{{.*}}:2: warning: "here is some warning about something"
// CHECK-NO-WARN-NOT: warning about something
// CHECK: diags_from_header.h:{{.*}}:2: error: "but this one is an error"
