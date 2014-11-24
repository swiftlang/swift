// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s
class A {
  var a : A?
  // CHECK: define {{.*}}1AcfMS0_FT_S0_
  init() {
    // This store should be part of the function prologue.
    // CHECK:   store {{.*}} %0, {{.*}}, align
    // CHECK-NOT: !dbg
    // CHECK: call {{.*}}llvm.dbg.declare
    println("Hi")
  }
}

let a = A()
