// Private discriminators should only be emitted for multi-file projects.

// RUN: %target-swift-frontend -emit-ir %s -g -o - | FileCheck --check-prefix=SINGLE %s
// SINGLE-NOT: -private-discriminator{{.*}}; [ DW_TAG_compile_unit ]

// RUN: %target-swift-frontend %S/../Inputs/empty.swift -primary-file %s -emit-ir -g | FileCheck %s
// CHECK: -private-discriminator [[DISCRIMINATOR:_[A-Z0-9]+]]{{.*}}; [ DW_TAG_compile_unit ]

private class A {
  init(val : Int) { member = val }
  private let member : Int
  // CHECK: [[DISCRIMINATOR]]{{.*}} [ DW_TAG_subprogram ] [line [[@LINE+1]]] [local] [def] [getMember]
  private func getMember() -> Int { return member }
  func getVal() -> Int { return getMember() }  
}

func f() {
  let a = A(val: 42)
  println(a.getVal())
}
