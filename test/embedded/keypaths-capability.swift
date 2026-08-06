// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -O -enable-experimental-feature Embedded -wmo -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-swift-frontend %s -Onone -enable-experimental-feature Embedded -wmo -c -o %t/main-onone.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main-onone.o -o %t/a-onone.out -dead_strip
// RUN: %target-run %t/a-onone.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

final class Ref { var v: Int32 = 0; var w: Int32 = 0 }

// `let` demotes the chain to read-only, then a mutable *class* stored property
// re-promotes it. Before the fix the object's isa said `KeyPath` while the
// static type said `ReferenceWritableKeyPath`.
struct LetThenClass { let r = Ref() }

// Same shape reached through a mutable struct property rather than a `let`.
struct VarThenClass { var r = Ref() }

// Two class hops: the second must not be held back by the first.
final class Outer2 { var inner = Ref() }
struct HoldsOuter { let o = Outer2() }

@inline(never) func kpLet() -> ReferenceWritableKeyPath<LetThenClass, Int32> {
  \LetThenClass.r.v
}
@inline(never) func kpVar() -> ReferenceWritableKeyPath<VarThenClass, Int32> {
  \VarThenClass.r.v
}
@inline(never) func kpTwo() -> ReferenceWritableKeyPath<HoldsOuter, Int32> {
  \HoldsOuter.o.inner.v
}

// Going through `WritableKeyPath` makes `swift_modifyAtWritableKeyPath`
// dispatch on the isa's `kind`, which is what actually trapped.
@inline(never) func kpLetW() -> WritableKeyPath<LetThenClass, Int32> {
  \LetThenClass.r.v
}

let a = LetThenClass()
a[keyPath: kpLet()] = 42
print(a.r.v == 42 ? "OK!" : "FAIL")   // CHECK: OK!

var aw = LetThenClass()
aw[keyPath: kpLetW()] = 7
print(aw.r.v == 7 ? "OK!" : "FAIL")   // CHECK-NEXT: OK!

let b = VarThenClass()
b[keyPath: kpVar()] = 5
print(b.r.v == 5 ? "OK!" : "FAIL")    // CHECK-NEXT: OK!

let c = HoldsOuter()
c[keyPath: kpTwo()] = 3
print(c.o.inner.v == 3 ? "OK!" : "FAIL")  // CHECK-NEXT: OK!

// The isa really is reference-writable, not just the static type.
let erased: AnyKeyPath = kpLet()
print(erased is ReferenceWritableKeyPath<LetThenClass, Int32> ? "OK!" : "FAIL")
// CHECK-NEXT: OK!

// A `let` on a *struct* still demotes: no class boundary means no promotion.
struct AllLet { let x: Int32 = 1 }
@inline(never) func kpRO() -> KeyPath<AllLet, Int32> { \AllLet.x }
print(AllLet()[keyPath: kpRO()] == 1 ? "OK!" : "FAIL")  // CHECK-NEXT: OK!
