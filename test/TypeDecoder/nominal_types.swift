// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/nominal_types -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/nominal_types -type-from-mangled=%t/input | %FileCheck %s

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

// DEMANGLE: $s13nominal_types5OuterVD
// CHECK: Outer

// DEMANGLE: $s13nominal_types5OuterV5InnerOD
// CHECK: Outer.Inner

// DEMANGLE: $s13nominal_types5OuterV12GenericInnerOy_SiSSGD
// DEMANGLE: $s13nominal_types5OuterV12GenericInnerOy_xq_GD
// CHECK: Outer.GenericInner<Int, String>
// CHECK: Outer.GenericInner<τ_0_0, τ_0_1>

// DEMANGLE: $s13nominal_types12GenericOuterO5InnerVyxq__GD
// DEMANGLE: $s13nominal_types12GenericOuterO0C5InnerVyxq__qd__qd_0_GD
// CHECK: GenericOuter<τ_0_0, τ_0_1>.Inner
// CHECK: GenericOuter<τ_0_0, τ_0_1>.GenericInner<τ_1_0, τ_1_1>

// DEMANGLE: $s13nominal_types12GenericOuterO5InnerVySiSS_GD
// DEMANGLE: $s13nominal_types12GenericOuterO0C5InnerVySiSS_SfSdGD
// CHECK: GenericOuter<Int, String>.Inner
// CHECK: GenericOuter<Int, String>.GenericInner<Float, Double>

// DEMANGLE: $s13nominal_types12GenericOuterOyxq_GD
// DEMANGLE: $s13nominal_types12GenericOuterOySiSSGD
// CHECK: GenericOuter<τ_0_0, τ_0_1>
// CHECK: GenericOuter<Int, String>

// DEMANGLE: $s13nominal_types11ConstrainedVyxGD
// CHECK: Constrained<τ_0_0>
