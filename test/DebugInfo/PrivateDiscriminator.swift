// Private discriminators should only be emitted for multi-file projects.

// RUN: %target-swift-frontend -emit-ir %s -g -o - | %FileCheck --check-prefix=SINGLE %s
// SINGLE-NOT: !DICompileUnit({{.*}}-private-discriminator

// RUN: %target-swift-frontend %S/../Inputs/empty.swift -primary-file %s -emit-ir -g | %FileCheck %s
// CHECK: !DICompileUnit({{.*}}flags: {{[^,]*}}-private-discriminator [[DISCRIMINATOR:_[A-Z0-9]+]]

func markUsed<T>(_ t: T) {}

private class A {
  init(val : Int64) { member = val }
  private let member : Int64
  // CHECK: !DISubprogram(name: "getMember"
  // CHECK-SAME:          linkageName: "{{[^"]*}}[[DISCRIMINATOR]]
  // CHECK-SAME:          line: [[@LINE+2]]
  // CHECK-SAME:          isLocal: true, isDefinition: true 
  private func getMember() -> Int64 { return member }
  func getVal() -> Int64 { return getMember() }  
}

func f() {
  let a = A(val: 42)
  markUsed(a.getVal())
}
