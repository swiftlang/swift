// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// CHECK: !DILocalVariable(name: "x4", arg: 4
// CHECK: !DILocalVariable(name: "_0", arg: 1
// CHECK: !DILocalVariable(name: "_1", arg: 2
// CHECK: !DILocalVariable(name: "_2", arg: 3

public func fourth<T>(_: T, _: T, _: T, x4 : T) -> T {
  return x4
}
