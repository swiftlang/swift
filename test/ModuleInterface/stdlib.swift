// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -parse-stdlib
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK-NOT: import Builtin

// CHECK: func test() {
// CHECK-NEXT: Builtin.sizeof
// CHECK-NEXT: {{^}$}}
@inlinable public func test() {
  Builtin.sizeof(Builtin.Int8.self)
}
