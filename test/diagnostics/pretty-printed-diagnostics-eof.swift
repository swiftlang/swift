// RUN: not %target-swift-frontend -diagnostic-style=swift -typecheck %/s 2>&1 | %FileCheck %s

// REQUIRES: swift_swift_parser

// CHECK:      {{SOURCE_DIR[/\]test[/\]diagnostics[/\]pretty-printed-diagnostics-eof\.swift}}:[[#LINE:]]:1: error: expected '}' in struct
// CHECK:      [[#LINE-2]] | struct MyStruct {
// CHECK-NEXT:             |                 `- note: to match this opening '{'
// CHECK-NEXT: [[#LINE-1]] |   func foo() {}
// CHECK-NEXT: [[#LINE]]   |
// CHECK-NEXT:             | `- error: expected '}' in struct

struct MyStruct {
  func foo() {}
