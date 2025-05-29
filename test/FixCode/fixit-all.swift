// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -fixit-all -emit-fixits-path %t.remap %s 2>&1 | %FileCheck %s

// CHECK: <unknown>:0: warning: ignoring '-fixit-all'; this option is obsolete
// CHECK: <unknown>:0: warning: ignoring '-emit-fixits-path'; this option is obsolete
