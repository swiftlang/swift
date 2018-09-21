// RUN: %target-swift-frontend %s -emit-ir -verify -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class C<A> {
  // CHECK: ![[A:.*]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}}identifier: "$sxD"
  // CHECK: !DILocalVariable(name: "x", arg: 1,
  // CHECK-SAME:             line: [[@LINE+7]],
  // CHECK-SAME:             type: ![[A]]
  // CHECK: !DILocalVariable(name: "y", arg: 2,
  // CHECK-SAME:             line: [[@LINE+4]],
  // CHECK-SAME:             type: ![[B:[0-9]+]]
  // CHECK: ![[B]] = !DICompositeType(tag: DW_TAG_structure_type,
  // CHECK-SAME:             identifier: "$sqd__D"
  func foo<B>(_ x: A, y :B) {
    markUsed("hello world")
  }
}

C<Int64>().foo(1, y: 3.14);
