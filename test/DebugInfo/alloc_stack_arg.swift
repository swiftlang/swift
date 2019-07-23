// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
public func snd<T, U>(_ t : (T, U)) -> U {
  let (_, y) = t
  return y
}

// Test that the alloc_stack's argument number is preserved.
// CHECK-NOT: !DILocalVariable(name: "t"
// CHECK: !DILocalVariable(name: "t", arg: 1
// CHECK-NOT: !DILocalVariable(name: "t"
