// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I %S/Inputs %s -enable-experimental-feature Embedded -cxx-interoperability-mode=default -wmo -parse-as-library -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -lc++ %target-embedded-posix-shim -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-swift-frontend -I %S/Inputs %s -enable-experimental-feature Embedded -cxx-interoperability-mode=default -wmo -O -parse-as-library -module-name main -c -o %t/main-opt.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main-opt.o -lc++ %target-embedded-posix-shim -o %t/a-opt.out
// RUN: %target-run %t/a-opt.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// Values of C++ foreign reference types stored in existentials and used as
// associated types in Embedded Swift.

import CxxForeignReference

protocol Describable {
  func describe() -> Int
}

// DerivedRef inherits this conformance.
extension BaseRef: Describable {
  func describe() -> Int { Int(value) }
}

protocol HasAssoc {
  associatedtype A
}

struct UsesBase: HasAssoc { typealias A = BaseRef }
struct UsesDerived: HasAssoc { typealias A = DerivedRef }

@inline(never)
func makeExistential(_ b: BaseRef) -> any Describable { b }

@inline(never)
func makeExistential(_ d: DerivedRef) -> any Describable { d }

@inline(never)
func makeAny(_ b: BaseRef) -> Any { b }

@inline(never)
func makeAny(_ d: DerivedRef) -> Any { d }

@inline(never)
func describeAll(_ values: [any Describable]) {
  for v in values {
    print(v.describe())
  }
}

@inline(never)
func assocIsBase<T: HasAssoc>(_: T.Type) -> Bool {
  T.A.self == BaseRef.self
}

@inline(never)
func test() {
  let b = createBaseRef(7)
  let d = createDerivedRef(4, 2)

  // CHECK: live objects: 2
  print("live objects: \(getLiveCount())")

  do {
    let e1 = makeExistential(b)
    let e2 = e1
    // CHECK-NEXT: 7
    print(e2.describe())
    _ = e1
  }

  // CHECK-NEXT: 7
  // CHECK-NEXT: 4
  describeAll([makeExistential(b), makeExistential(d)])

  let anyBase = makeAny(b)
  if let b2 = anyBase as? BaseRef {
    // CHECK-NEXT: cast Base to BaseRef: 7
    print("cast Base to BaseRef: \(b2.value)")
  }
  // CHECK-NEXT: cast Base to DerivedRef fails: true
  print("cast Base to DerivedRef fails: \((anyBase as? DerivedRef) == nil)")

  let anyDerived = makeAny(d)
  if let d2 = anyDerived as? DerivedRef {
    // CHECK-NEXT: cast Derived to DerivedRef: 2
    print("cast Derived to DerivedRef: \(d2.other)")
  }

  // CHECK-NEXT: UsesBase.A is BaseRef: true
  print("UsesBase.A is BaseRef: \(assocIsBase(UsesBase.self))")
  // CHECK-NEXT: UsesDerived.A is BaseRef: false
  print("UsesDerived.A is BaseRef: \(assocIsBase(UsesDerived.self))")
}

@main
struct Main {
  static func main() {
    test()
    // CHECK-NEXT: live objects after test: 0
    print("live objects after test: \(getLiveCount())")
  }
}
