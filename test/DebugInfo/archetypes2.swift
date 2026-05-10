// RUN: %target-swift-frontend %s -emit-ir -verify -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class C<A> {
  // CHECK-DAG: !DILocalVariable(name: "x", arg: 1,{{.*}}line: [[@LINE+6]], {{.*}}type: ![[LET_A:[0-9]+]]
  // CHECK-DAG: ![[LET_A]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[A:[0-9]+]])
  // CHECK-DAG: ![[A]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}}name: "$sxD"
  // CHECK: !DILocalVariable(name: "y", arg: 2,{{.*}}line: [[@LINE+3]],{{.*}}type: ![[LET_B:[0-9]+]]
  // CHECK-DAG: ![[LET_B]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[B:[0-9]+]])
  // CHECK-DAG: ![[B]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}}name: "$sqd__D"
  func foo<B>(_ x: A, y :B) {
    markUsed("hello world")
  }
}

C<Int64>().foo(1, y: 3.14);
