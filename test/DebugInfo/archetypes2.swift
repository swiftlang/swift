// RUN: %target-swift-frontend %s -emit-ir -verify -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class C<A> {
  // CHECK: ![[A:.*]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}}identifier: "$sxD"
  // CHECK: ![[B:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}}identifier: "$sqd__D")
  // CHECK: !DILocalVariable(name: "x", arg: 1,{{.*}}line: [[@LINE+2]],{{.*}}type: ![[A]]
  // CHECK: !DILocalVariable(name: "y", arg: 2,{{.*}}line: [[@LINE+1]],{{.*}}type: ![[B]])
  func foo<B>(_ x: A, y :B) {
    markUsed("hello world")
  }
}

C<Int64>().foo(1, y: 3.14);
