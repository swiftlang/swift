// Private discriminators should only be emitted for multi-file projects.

// RUN: %target-swift-frontend -emit-ir %s -g -o - | FileCheck --check-prefix=SINGLE %s
// SINGLE-NOT: !MDCompileUnit({{.*}}-private-discriminator

// RUN: %target-swift-frontend %S/../Inputs/empty.swift -primary-file %s -emit-ir -g | FileCheck %s
// CHECK: !MDCompileUnit({{.*}}flags: {{[^,]*}}-private-discriminator [[DISCRIMINATOR:_[A-Z0-9]+]]

private class A {
  init(val : Int) { member = val }
  private let member : Int
  // CHECK: !MDSubprogram(name: "getMember"
  // CHECK-SAME:          linkageName: "{{[^"]*}}[[DISCRIMINATOR]]
  // CHECK-SAME:          line: [[@LINE+2]]
  // CHECK-SAME:          isLocal: true, isDefinition: true 
  private func getMember() -> Int { return member }
  func getVal() -> Int { return getMember() }  
}

func f() {
  let a = A(val: 42)
  println(a.getVal())
}
