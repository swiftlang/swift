// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

class A {
  var a : A?
  // CHECK: define {{.*}}1AcfMS0_FT_S0_
  init() {
    // This store should be part of the function prologue.
    // CHECK:   store {{.*}} %0, {{.*}}, align
    // CHECK-NOT: !dbg
    // CHECK: call {{.*}}llvm.dbg.declare
    markUsed("Hi")
  }
}

let a = A()
