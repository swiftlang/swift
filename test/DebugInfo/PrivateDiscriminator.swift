// Private discriminators should only be emitted for multi-file projects.

// RUN: %target-swift-frontend -emit-ir %s -g -o - \
// RUN:   | %FileCheck --check-prefix=SINGLE %s
// SINGLE-NOT: !DICompileUnit({{.*}}-private-discriminator

// RUN: %target-swift-frontend %S/../Inputs/empty.swift -primary-file %s \
// RUN:   -emit-ir -g | %FileCheck %s
// CHECK: !DICompileUnit({{.*}}flags: {{[^,]*}}-private-discriminator [[DISCRIMINATOR:_[A-Z0-9]+]]
// CHECK: ![[MOD:.*]] = !DIModule(scope: null, name: "empty"
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "InA",
// CHECK-SAME:             scope: ![[A:[0-9]+]],
// CHECK: ![[A]] = !DICompositeType(tag: DW_TAG_structure_type, name: "A",
// CHECK-SAME:                      scope: ![[NS:[0-9]+]]
// CHECK: !DINamespace(name: "[[DISCRIMINATOR]]",
// CHECK-SAME:         scope: ![[OUTER:[0-9]+]], exportSymbols: true)
// CHECK: ![[OUTER]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:                          name: "Outer",
// CHECK-SAME:                          scope: ![[MOD]],

func markUsed<T>(_ t: T) {}

public class Outer {
  fileprivate class A {
    fileprivate struct InA {
      let i : Int64
      init(_ val : Int64) { i = val }
    }
    
    init(val : Int64) { member = InA(val) }
    fileprivate let member : InA
    // CHECK: !DISubprogram(name: "getMember"
    // CHECK-SAME:          linkageName: "{{[^"]*}}[[DISCRIMINATOR]]
    // CHECK-SAME:          line: [[@LINE+2]]
    // CHECK-SAME:          spFlags: DISPFlagLocalToUnit | DISPFlagDefinition
    fileprivate func getMember() -> Int64 { return member.i }
  }
}

// CHECK: ![[G:[0-9]+]] = distinct !DISubprogram(name: "g",
// CHECK-SAME:                                   scope: ![[MOD]]
fileprivate func g() -> Int64 {
  // CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "InG",
  // CHECK-SAME:             scope: ![[G]],
  struct InG {
    let i : Int64
    init(_ val : Int64) { i = val }
  }

  return InG(42).i
}

// CHECK: distinct !DISubprogram(name: "f", {{.*}}, scope: ![[MOD]]
public func f() {
  let a = Outer.A(val: g())
  markUsed(a)
}
