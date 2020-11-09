// RUN: %target-run-simple-swift
// RUN: %target-swift-emit-sil -Xllvm -debug-only=differentiation -o /dev/null 2>&1 %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: asserts

// Test differentiation of `Optional` properties.

import DifferentiationUnittest
import StdlibUnittest

var OptionalTests = TestSuite("OptionalPropertyDifferentiation")

// Test `Optional` struct stored properties.

struct Struct: Differentiable {
  var stored: Float
  var optional: Float?

  @differentiable
  func method() -> Float {
    let s: Struct
    do {
      let tmp = Struct(stored: stored, optional: optional)
      let tuple = (tmp, tmp)
      s = tuple.0
    }
    if let x = s.optional {
      return x * s.stored
    }
    return s.stored
  }
}

// Check active SIL instructions in representative original functions.
// This tests SIL instruction coverage of derivative function cloners (e.g. PullbackCloner).

// CHECK-LABEL: [AD] Activity info for ${{.*}}Struct{{.*}}method{{.*}} at (parameters=(0) results=(0))
// CHECK:   [ACTIVE] {{.*}} struct_extract {{%.*}} : $Struct, #Struct.stored
// CHECK:   [ACTIVE] {{.*}} struct_extract {{%.*}} : $Struct, #Struct.optional
// CHECK:   [ACTIVE] {{.*}} tuple ({{%.*}} : $Struct, {{%.*}} : $Struct)
// CHECK:   [ACTIVE] {{.*}} destructure_tuple {{%.*}} : $(Struct, Struct)
// CHECK:   [ACTIVE] {{.*}} struct_element_addr {{%.*}} : $*Struct, #Struct.optional
// CHECK:   [ACTIVE] {{.*}} struct_element_addr {{%.*}} : $*Struct, #Struct.stored
// CHECK-LABEL: End activity info for ${{.*}}Struct{{.*}}method{{.*}} at (parameters=(0) results=(0))

// CHECK-LABEL: [AD] Activity info for $s4null6StructV6stored8optionalACSf_SfSgtcfC at (parameters=(0 1) results=(0))
// CHECK:   [ACTIVE]   {{%.*}} struct $Struct ({{%.*}} : $Float, {{%.*}} : $Optional<Float>)
// CHECK-LABEL: End activity info for $s4null6StructV6stored8optionalACSf_SfSgtcfC at (parameters=(0 1) results=(0))

struct StructTracked: Differentiable {
  var stored: NonresilientTracked<Float>
  var optional: NonresilientTracked<Float>?

  @differentiable
  func method() -> NonresilientTracked<Float> {
    let s: StructTracked
    do {
      let tmp = StructTracked(stored: stored, optional: optional)
      let tuple = (tmp, tmp)
      s = tuple.0
    }
    if let x = s.optional {
      return x * s.stored
    }
    return s.stored
  }
}

struct StructGeneric<T: Differentiable>: Differentiable {
  var stored: T
  var optional: T?

  @differentiable
  func method() -> T {
    let s: StructGeneric
    do {
      let tmp = StructGeneric(stored: stored, optional: optional)
      let tuple = (tmp, tmp)
      s = tuple.0
    }
    if let x = s.optional {
      return x
    }
    return s.stored
  }
}

OptionalTests.test("Optional struct stored properties") {
  expectEqual(
    valueWithGradient(at: Struct(stored: 3, optional: 4), in: { $0.method() }),
    (12, .init(stored: 4, optional: .init(3))))
  expectEqual(
    valueWithGradient(at: Struct(stored: 3, optional: nil), in: { $0.method() }),
    (3, .init(stored: 1, optional: .init(0))))

  expectEqual(
    valueWithGradient(at: StructTracked(stored: 3, optional: 4), in: { $0.method() }),
    (12, .init(stored: 4, optional: .init(3))))
  expectEqual(
    valueWithGradient(at: StructTracked(stored: 3, optional: nil), in: { $0.method() }),
    (3, .init(stored: 1, optional: .init(0))))

  expectEqual(
    valueWithGradient(at: StructGeneric<Float>(stored: 3, optional: 4), in: { $0.method() }),
    (4, .init(stored: 0, optional: .init(1))))
  expectEqual(
    valueWithGradient(at: StructGeneric<Float>(stored: 3, optional: nil), in: { $0.method() }),
    (3, .init(stored: 1, optional: .init(0))))
}

// Test `Optional` class stored properties.

struct Class: Differentiable {
  var stored: Float
  var optional: Float?

  init(stored: Float, optional: Float?) {
    self.stored = stored
    self.optional = optional
  }

  @differentiable
  func method() -> Float {
    let c: Class
    do {
      let tmp = Class(stored: stored, optional: optional)
      let tuple = (tmp, tmp)
      c = tuple.0
    }
    if let x = c.optional {
      return x * c.stored
    }
    return c.stored
  }
}

struct ClassTracked: Differentiable {
  var stored: NonresilientTracked<Float>
  var optional: NonresilientTracked<Float>?

  init(stored: NonresilientTracked<Float>, optional: NonresilientTracked<Float>?) {
    self.stored = stored
    self.optional = optional
  }

  @differentiable
  func method() -> NonresilientTracked<Float> {
    let c: ClassTracked
    do {
      let tmp = ClassTracked(stored: stored, optional: optional)
      let tuple = (tmp, tmp)
      c = tuple.0
    }
    if let x = c.optional {
      return x * c.stored
    }
    return c.stored
  }
}

struct ClassGeneric<T: Differentiable>: Differentiable {
  var stored: T
  var optional: T?

  init(stored: T, optional: T?) {
    self.stored = stored
    self.optional = optional
  }

  @differentiable
  func method() -> T {
    let c: ClassGeneric
    do {
      let tmp = ClassGeneric(stored: stored, optional: optional)
      let tuple = (tmp, tmp)
      c = tuple.0
    }
    if let x = c.optional {
      return x
    }
    return c.stored
  }
}

OptionalTests.test("Optional class stored properties") {
  expectEqual(
    valueWithGradient(at: Class(stored: 3, optional: 4), in: { $0.method() }),
    (12, .init(stored: 4, optional: .init(3))))
  expectEqual(
    valueWithGradient(at: Class(stored: 3, optional: nil), in: { $0.method() }),
    (3, .init(stored: 1, optional: .init(0))))

  expectEqual(
    valueWithGradient(at: ClassTracked(stored: 3, optional: 4), in: { $0.method() }),
    (12, .init(stored: 4, optional: .init(3))))
  expectEqual(
    valueWithGradient(at: ClassTracked(stored: 3, optional: nil), in: { $0.method() }),
    (3, .init(stored: 1, optional: .init(0))))

  expectEqual(
    valueWithGradient(at: ClassGeneric<Tracked<Float>>(stored: 3, optional: 4), in: { $0.method() }),
    (4, .init(stored: 0, optional: .init(1))))
  expectEqual(
    valueWithGradient(at: ClassGeneric<Tracked<Float>>(stored: 3, optional: nil), in: { $0.method() }),
    (3, .init(stored: 1, optional: .init(0))))
}

runAllTests()
