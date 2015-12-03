// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
class C {
  func withClosure(_ : () -> ()) -> () {}

  func f() {
    // There should not be a local weak variable "self" shadowing the
    // implicit self argument.
    // CHECK: DILocalVariable(name: "self", arg: 1
    // CHECK-NOT: DILocalVariable(name: "self", scope
    withClosure { [weak self] in
      guard let s = self else { return }
    }
  }
}
