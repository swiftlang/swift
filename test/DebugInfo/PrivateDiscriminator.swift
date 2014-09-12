// RUN: %swift -emit-ir %s -g -o - | FileCheck %s
// RUN: %swift %S/../Inputs/empty.swift -primary-file %s -emit-ir -g | FileCheck %s
// CHECK: -private-discriminator [[DISCRIMINATOR:_[A-Z0-9]+]]{{.*}}; [ DW_TAG_compile_unit ]
class A {
  init(val : Int) { member = val }
  private let member : Int
  // CHECK: [[DISCRIMINATOR]]{{.*}} [ DW_TAG_subprogram ] [line [[@LINE+1]]] [def] [getMember]
  private func getMember() -> Int { return member }
  func getVal() -> Int { return getMember() }  
}

func f() {
  let a = A(val: 42)
  println(a.getVal())
}
