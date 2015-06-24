// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

// Don't crash when emitting debug info for anonymous variables.
// CHECK: !DILocalVariable({{.*}} name: "_"

func markUsed<T>(t: T) {}

protocol F_ {
  func successor() -> Self
}

protocol F : F_ {
  func ~> (_: Self, _: (_Distance, (Self))) -> Int
}

struct _Distance {}

func ~> <I: F_>(self_:I, _: (_Distance, (I))) -> Int {
  self_.successor()
  markUsed("F")
  return 0
}
