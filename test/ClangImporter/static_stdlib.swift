// RUN: %swift_frontend_plain -parse-stdlib -typecheck %s -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-DYNAMIC
// RUN: %swift_frontend_plain -parse-stdlib -use-static-resource-dir -typecheck %s -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-STATIC

// CHECK-DYNAMIC-NOT: clang importer cc1 args: {{.*}} '-D' 'SWIFT_STATIC_STDLIB'
// CHECK-STATIC: clang importer cc1 args: {{.*}} '-D' 'SWIFT_STATIC_STDLIB'
