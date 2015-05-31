// RUN: %target-swift-frontend %s -emit-ir -verify -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

class C<A> {
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "x", {{.*}}line: [[@LINE+2]],{{.*}}type: !"_TtQq_C11archetypes21C"
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "y", {{.*}}line: [[@LINE+1]],{{.*}}type: !"_TtQq_FC11archetypes21C3foo
  func foo <B> ( x : A,  y : B) {
    markUsed("hello world")
  }
}

C<Int>().foo(1, y: 3.14);
