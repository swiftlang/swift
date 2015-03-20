// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s
// XFAIL: *

// Don't crash when emitting debug info for anonymous variables.
// CHECK: !MDLocalVariable({{.*}} name: "_"
protocol F_ {
  func successor() -> Self
}

protocol F : F_ {
  func ~> (Self, (_Distance, (Self))) -> Int
}

struct _Distance {}

func ~> <I: F_>(self_:I, (_Distance, (I))) -> Int {
  self_.successor()
  println("F")
  return 0
}
