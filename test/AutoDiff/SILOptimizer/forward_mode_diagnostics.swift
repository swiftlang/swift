// RUN: %target-swift-frontend -enable-experimental-forward-mode-differentiation -emit-sil -verify %s

// Test forward-mode differentiation transform diagnostics.

// TODO: Move these tests back into `autodiff_diagnostics.swift` once
// forward mode reaches feature parity with reverse mode.

import _Differentiation

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

@differentiable
func basic(_ x: Float) -> Float {
  return x
}

//===----------------------------------------------------------------------===//
// Control flow
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{forward-mode differentiation does not yet support control flow}}
func cond(_ x: Float) -> Float {
  if x > 0 {
    return x * x
  }
  return x + x
}

//===----------------------------------------------------------------------===//
// Non-varied results
//===----------------------------------------------------------------------===//

@differentiable
func nonVariedResult(_ x: Float) -> Float {
  // TODO(TF-788): Re-enable non-varied result warning.
  // xpected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}} {{15-15=)}}
  return 0
}

//===----------------------------------------------------------------------===//
// Multiple results
//===----------------------------------------------------------------------===//

// TODO(TF-983): Support differentiation of multiple results.
/*
func multipleResults(_ x: Float) -> (Float, Float) {
  return (x, x)
}
@differentiable
func usesMultipleResults(_ x: Float) -> Float {
  let tuple = multipleResults(x)
  return tuple.0 + tuple.1
}
*/

//===----------------------------------------------------------------------===//
// `inout` parameter differentiation
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutParamNonactiveInitialResult(_ x: Float) -> Float {
  var result: Float = 1
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  result += x
  return result
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutParamTuple(_ x: Float) -> Float {
  var tuple = (x, x)
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  tuple.0 *= x
  return x * tuple.0
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{forward-mode differentiation does not yet support control flow}}
func activeInoutParamControlFlow(_ array: [Float]) -> Float {
  var result: Float = 1
  for i in withoutDerivative(at: array).indices {
    result += array[i]
  }
  return result
}

struct Mut: Differentiable {}
extension Mut {
  @differentiable(wrt: x)
  mutating func mutatingMethod(_ x: Mut) {}
}

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func nonActiveInoutParam(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  return nonactive.mutatingMethod(x)
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutParamMutatingMethod(_ x: Mut) -> Mut {
  var result = x
  result = result.mutatingMethod(result)
  return result
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutParamMutatingMethodVar(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = nonactive
  result = result.mutatingMethod(x)
  return result
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutParamMutatingMethodTuple(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = (nonactive, x)
  let result2 = result.0.mutatingMethod(result.0)
  return result2
}
*/

//===----------------------------------------------------------------------===//
// Subset parameter differentiation thunks
//===----------------------------------------------------------------------===//

// FIXME(SR-13046): Non-differentiability diagnostic crash due to invalid source location.
/*
func testNoDerivativeParameter(_ f: @differentiable (Float, @noDerivative Float) -> Float) -> Float {
  return derivative(at: 2, 3) { (x, y) in f(x * x, y) }
}
*/

//===----------------------------------------------------------------------===//
// Stored property access differentiation
//===----------------------------------------------------------------------===//

// Test differentiation of invalid stored property access instructions:
// `struct_extract`, `struct_element_addr`, `ref_element_addr`.

struct StructTangentVectorNotStruct: Differentiable {
  var x: Float

  enum TangentVector: Differentiable, AdditiveArithmetic {
    case x(Float)
    typealias TangentVector = Self
    static func ==(_: Self, _: Self) -> Bool { fatalError() }
    static var zero: Self { fatalError() }
    static func +(_: Self, _: Self) -> Self { fatalError() }
    static func -(_: Self, _: Self) -> Self { fatalError() }
  }
  mutating func move(along direction: TangentVector) {}
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+3 {{when differentiating this function definition}}
@differentiable
@_silgen_name("test_struct_tangent_vector_not_struct")
func testStructTangentVectorNotStruct(_ s: StructTangentVectorNotStruct) -> Float {
  // expected-note @+1 {{cannot differentiate access to property 'StructTangentVectorNotStruct.x' because 'StructTangentVectorNotStruct.TangentVector' is not a struct}}
  return s.x
}

// CHECK-LABEL: sil {{.*}} @test_struct_tangent_vector_not_struct
// CHECK: struct_extract {{%.*}} : $StructTangentVectorNotStruct, #StructTangentVectorNotStruct.x

struct StructOriginalPropertyNotDifferentiable: Differentiable {
  struct Nondiff {
    var x: Float
  }
  var nondiff: Nondiff

  struct TangentVector: Differentiable & AdditiveArithmetic {
    var nondiff: Float
  }
  mutating func move(along direction: TangentVector) {}
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+3 {{when differentiating this function definition}}
@differentiable
@_silgen_name("test_struct_original_property_not_differentiable")
func testStructOriginalPropertyNotDifferentiable(_ s: StructOriginalPropertyNotDifferentiable) -> Float {
  // expected-note @+1 {{cannot differentiate access to property 'StructOriginalPropertyNotDifferentiable.nondiff' because property type 'StructOriginalPropertyNotDifferentiable.Nondiff' does not conform to 'Differentiable'}}
  return s.nondiff.x
}

// CHECK-LABEL: sil {{.*}} @test_struct_original_property_not_differentiable
// CHECK: struct_extract {{%.*}} : $StructOriginalPropertyNotDifferentiable, #StructOriginalPropertyNotDifferentiable.nondiff

struct StructTangentVectorPropertyNotFound: Differentiable {
  var x: Float

  struct TangentVector: Differentiable, AdditiveArithmetic {
    var y: Float
  }
  mutating func move(along direction: TangentVector) {}
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+3 {{when differentiating this function definition}}
@differentiable
@_silgen_name("test_struct_tangent_property_not_found")
func testStructTangentPropertyNotFound(_ s: StructTangentVectorPropertyNotFound) -> Float {
  // expected-warning @+1 {{variable 'tmp' was never mutated}}
  var tmp = s
  // expected-note @+1 {{cannot differentiate access to property 'StructTangentVectorPropertyNotFound.x' because 'StructTangentVectorPropertyNotFound.TangentVector' does not have a stored property named 'x'}}
  return tmp.x
}

// CHECK-LABEL: sil {{.*}} @test_struct_tangent_property_not_found
// CHECK: struct_element_addr {{%.*}} : $*StructTangentVectorPropertyNotFound, #StructTangentVectorPropertyNotFound.x

struct StructTangentPropertyWrongType: Differentiable {
  var x: Float

  struct TangentVector: Differentiable, AdditiveArithmetic {
    var x: Double
  }
  mutating func move(along direction: TangentVector) {}
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+3 {{when differentiating this function definition}}
@differentiable
@_silgen_name("test_struct_tangent_property_wrong_type")
func testStructTangentPropertyWrongType(_ s: StructTangentPropertyWrongType) -> Float {
  // expected-warning @+1 {{variable 'tmp' was never mutated}}
  var tmp = s
  // expected-note @+1 {{cannot differentiate access to property 'StructTangentPropertyWrongType.x' because 'StructTangentPropertyWrongType.TangentVector.x' does not have expected type 'Float.TangentVector' (aka 'Float')}}
  return tmp.x
}

// CHECK-LABEL: sil {{.*}} @test_struct_tangent_property_wrong_type
// CHECK: struct_element_addr {{%.*}} : $*StructTangentPropertyWrongType, #StructTangentPropertyWrongType.x

final class ClassTangentPropertyWrongType: Differentiable {
  var x: Float = 0

  struct TangentVector: Differentiable, AdditiveArithmetic {
    var x: Double
  }
  func move(along direction: TangentVector) {}
}

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable
@_silgen_name("test_class_tangent_property_wrong_type")
func testClassTangentPropertyWrongType(_ c: ClassTangentPropertyWrongType) -> Float {
  var tmp = c
  return tmp.x
}
*/

// CHECK-LABEL: sil {{.*}} @test_class_tangent_property_wrong_type
// CHECK: ref_element_addr {{%.*}} : $ClassTangentPropertyWrongType, #ClassTangentPropertyWrongType.x

struct StructTangentPropertyNotStored: Differentiable {
  var x: Float

  struct TangentVector: Differentiable, AdditiveArithmetic {
    var x: Float { 0 }
  }
  mutating func move(along direction: TangentVector) {}
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+3 {{when differentiating this function definition}}
@differentiable
@_silgen_name("test_struct_tangent_property_not_stored")
func testStructTangentPropertyNotStored(_ s: StructTangentPropertyNotStored) -> Float {
  // expected-warning @+1 {{variable 'tmp' was never mutated}}
  var tmp = s
  // expected-note @+1 {{cannot differentiate access to property 'StructTangentPropertyNotStored.x' because 'StructTangentPropertyNotStored.TangentVector.x' is not a stored property}}
  return tmp.x
}

// CHECK-LABEL: sil {{.*}} @test_struct_tangent_property_not_stored
// CHECK: struct_element_addr {{%.*}} : $*StructTangentPropertyNotStored, #StructTangentPropertyNotStored.x

final class ClassTangentPropertyNotStored: Differentiable {
  var x: Float = 0

  struct TangentVector: Differentiable, AdditiveArithmetic {
    var x: Float { 0 }
  }
  func move(along direction: TangentVector) {}
}

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable
@_silgen_name("test_class_tangent_property_not_stored")
func testClassTangentPropertyNotStored(_ c: ClassTangentPropertyNotStored) -> Float {
  var tmp = c
  return tmp.x
}
*/

// CHECK-LABEL: sil {{.*}} @test_class_tangent_property_not_stored
// CHECK: ref_element_addr {{%.*}} : $ClassTangentPropertyNotStored, #ClassTangentPropertyNotStored.x
