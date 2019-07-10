// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/nominal_types -emit-module

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/nominal_types -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/nominal_types -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

struct Outer  {
  enum Inner {
    case a
    init() { fatalError() }
  }
  enum GenericInner<T, U> {
    case a
    init() { fatalError() }
  }
}

enum GenericOuter<T, U> {
  case a
  init() { fatalError() }

  struct Inner {}
  struct GenericInner<T, U> {}
}

func blackHole(_: Any...) {}

do {
  let x1 = Outer()
  let x2 = Outer.Inner()
  let x3 = Outer.GenericInner<Int, String>()

  blackHole(x1, x2, x3)
}

do {
  let x1 = GenericOuter<Int, String>()
  let x2 = GenericOuter<Int, String>.Inner()
  let x3 = GenericOuter<Int, String>.GenericInner<Float, Double>()

  blackHole(x1, x2, x3)
}

protocol P {}

struct Constrained<T : P> {}

func generic<T>(_: Constrained<T>) {}

// DEMANGLE-TYPE: $s13nominal_types5OuterVD
// CHECK-TYPE: Outer

// DEMANGLE-TYPE: $s13nominal_types5OuterV5InnerOD
// CHECK-TYPE: Outer.Inner

// DEMANGLE-TYPE: $s13nominal_types5OuterV12GenericInnerOy_SiSSGD
// DEMANGLE-TYPE: $s13nominal_types5OuterV12GenericInnerOy_xq_GD
// CHECK-TYPE: Outer.GenericInner<Int, String>
// CHECK-TYPE: Outer.GenericInner<τ_0_0, τ_0_1>

// DEMANGLE-TYPE: $s13nominal_types12GenericOuterO5InnerVyxq__GD
// DEMANGLE-TYPE: $s13nominal_types12GenericOuterO0C5InnerVyxq__qd__qd_0_GD
// CHECK-TYPE: GenericOuter<τ_0_0, τ_0_1>.Inner
// CHECK-TYPE: GenericOuter<τ_0_0, τ_0_1>.GenericInner<τ_1_0, τ_1_1>

// DEMANGLE-TYPE: $s13nominal_types12GenericOuterO5InnerVySiSS_GD
// DEMANGLE-TYPE: $s13nominal_types12GenericOuterO0C5InnerVySiSS_SfSdGD
// CHECK-TYPE: GenericOuter<Int, String>.Inner
// CHECK-TYPE: GenericOuter<Int, String>.GenericInner<Float, Double>

// DEMANGLE-TYPE: $s13nominal_types12GenericOuterOyxq_GD
// DEMANGLE-TYPE: $s13nominal_types12GenericOuterOySiSSGD
// CHECK-TYPE: GenericOuter<τ_0_0, τ_0_1>
// CHECK-TYPE: GenericOuter<Int, String>

// DEMANGLE-TYPE: $s13nominal_types11ConstrainedVyxGD
// CHECK-TYPE: Constrained<τ_0_0>


// DEMANGLE-DECL: $s13nominal_types5OuterV
// DEMANGLE-DECL: $s13nominal_types5OuterV5InnerO
// DEMANGLE-DECL: $s13nominal_types5OuterV12GenericInnerO
// DEMANGLE-DECL: $s13nominal_types12GenericOuterO
// DEMANGLE-DECL: $s13nominal_types12GenericOuterO5InnerV
// DEMANGLE-DECL: $s13nominal_types12GenericOuterO0C5InnerV
// DEMANGLE-DECL: $s13nominal_types1PP
// DEMANGLE-DECL: $s13nominal_types11ConstrainedV

// CHECK-DECL: nominal_types.(file).Outer
// CHECK-DECL: nominal_types.(file).Outer.Inner
// CHECK-DECL: nominal_types.(file).Outer.GenericInner
// CHECK-DECL: nominal_types.(file).GenericOuter
// CHECK-DECL: nominal_types.(file).GenericOuter.Inner
// CHECK-DECL: nominal_types.(file).GenericOuter.GenericInner
// CHECK-DECL: nominal_types.(file).P
// CHECK-DECL: nominal_types.(file).Constrained
