// REQUIRES: rdar26102242
// RUN: %target-swift-frontend %s -emit-ir -verify -g -o - | FileCheck %s

func markUsed<T>(_ t: T) {}

class C<A> {
// CHECK-DAG: !DILocalVariable(name: "x", arg: 1,{{.*}}line: [[@LINE+2]],{{.*}}type: !"_TtQq_C11archetypes21C"
// CHECK-DAG: !DILocalVariable(name: "y", arg: 2,{{.*}}line: [[@LINE+1]],{{.*}}type: !"_TtQq_FC11archetypes21C3foo
  func foo<B>(_ x: A, y :B) {
    markUsed("hello world")
  }
}

C<Int64>().foo(1, y: 3.14);
