// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -typecheck -verify %s

import _Differentiation

//===----------------------------------------------------------------------===//
// Basic @differentiable function types.
//===----------------------------------------------------------------------===//

// expected-error @+1 {{@differentiable attribute only applies to function types}}
let _: @differentiable Float

let _: @differentiable (Float) -> Float
let _: @differentiable (Float) throws -> Float

//===----------------------------------------------------------------------===//
// Type differentiability
//===----------------------------------------------------------------------===//

struct NonDiffType { var x: Int }

// FIXME: Properly type-check parameters and the result's differentiability
// expected-error @+1 {{parameter type 'NonDiffType' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
let _: @differentiable (NonDiffType) -> Float

// Emit `@noDerivative` fixit iff there is at least one valid differentiability parameter.
// expected-error @+1 {{parameter type 'NonDiffType' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'; did you want to add '@noDerivative' to this parameter?}} {{32-32=@noDerivative }}
let _: @differentiable (Float, NonDiffType) -> Float

// expected-error @+1 {{result type 'NonDiffType' does not conform to 'Differentiable' and satisfy 'NonDiffType == NonDiffType.TangentVector', but the enclosing function type is '@differentiable(linear)'}}
let _: @differentiable(linear) (Float) -> NonDiffType

// Emit `@noDerivative` fixit iff there is at least one valid linearity parameter.
// expected-error @+1 {{parameter type 'NonDiffType' does not conform to 'Differentiable' and satisfy 'NonDiffType == NonDiffType.TangentVector', but the enclosing function type is '@differentiable(linear)'; did you want to add '@noDerivative' to this parameter?}} {{40-40=@noDerivative }}
let _: @differentiable(linear) (Float, NonDiffType) -> Float

// expected-error @+1 {{result type 'NonDiffType' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
let _: @differentiable (Float) -> NonDiffType

// expected-error @+1 {{result type 'NonDiffType' does not conform to 'Differentiable' and satisfy 'NonDiffType == NonDiffType.TangentVector', but the enclosing function type is '@differentiable(linear)'}}
let _: @differentiable(linear) (Float) -> NonDiffType

let _: @differentiable(linear) (Float) -> Float

// expected-error @+1 {{result type '@differentiable (U) -> Float' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test1<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> @differentiable (U) -> Float) {}
// expected-error @+1 {{result type '(U) -> Float' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test2<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> (U) -> Float) {}
// expected-error @+2 {{result type 'Int' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
// expected-error @+1 {{result type '@differentiable (U) -> Int' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test3<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> @differentiable (U) -> Int) {}
// expected-error @+1 {{result type '(U) -> Int' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test4<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> (U) -> Int) {}

//===----------------------------------------------------------------------===//
// Parameter selection (@noDerivative)
//===----------------------------------------------------------------------===//

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: @noDerivative Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: (@noDerivative Float) -> Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: (@noDerivative Float, Float) -> Float

let _: @differentiable (Float, @noDerivative Float) -> Float // okay

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: (Float) -> @noDerivative Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: @differentiable (Float) -> @noDerivative Float

// expected-error @+2 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
// expected-error @+1 {{'@noDerivative' must not be used on variadic parameters}}
let _: (Float, @noDerivative Float...) -> Float

let _: @differentiable (@noDerivative Float, Float) -> Float

// expected-error @+1 {{'@noDerivative' must not be used on variadic parameters}}
let _: @differentiable (Float, @noDerivative Float...) -> Float

//===----------------------------------------------------------------------===//
// Inferred conformances
//===----------------------------------------------------------------------===//

let diffFunc: @differentiable (Float) -> Float
let linearFunc: @differentiable(linear) (Float) -> Float
func inferredConformances<T, U>(_: @differentiable (T) -> U) {}
func inferredConformancesLinear<T, U>(_: @differentiable(linear) (T) -> U) {}
inferredConformances(diffFunc)
inferredConformancesLinear(linearFunc)

func inferredConformancesResult<T, U>() -> @differentiable (T) -> U {}
func inferredConformancesResultLinear<T, U>() -> @differentiable(linear) (T) -> U {}

let diffFuncWithNondiff: @differentiable (Float, @noDerivative Int) -> Float
let linearFuncWithNondiff: @differentiable(linear) (Float, @noDerivative Int) -> Float
func inferredConformances<T, U, V>(_: @differentiable (T, @noDerivative U) -> V) {}
func inferredConformancesLinear<T, U, V>(_: @differentiable(linear) (T, @noDerivative U) -> V) {}
inferredConformances(diffFuncWithNondiff)
inferredConformancesLinear(linearFuncWithNondiff)

struct Vector<T> {
  var x, y: T
}
extension Vector: Equatable where T: Equatable {}
extension Vector: AdditiveArithmetic where T: AdditiveArithmetic {
  static var zero: Self { fatalError() }
  static func + (lhs: Self, rhs: Self) -> Self { fatalError() }
  static func - (lhs: Self, rhs: Self) -> Self { fatalError() }
}
extension Vector: Differentiable where T: Differentiable {
  struct TangentVector: Equatable, AdditiveArithmetic, Differentiable {
    var x, y: T.TangentVector
    static var zero: Self { fatalError() }
    static func + (lhs: Self, rhs: Self) -> Self { fatalError() }
    static func - (lhs: Self, rhs: Self) -> Self { fatalError() }
    typealias TangentVector = Self
  }
  mutating func move(along direction: TangentVector) { fatalError() }
}

func inferredConformancesGeneric<T, U>(_: @differentiable (Vector<T>) -> Vector<U>) {}

// expected-error @+4 {{generic signature requires types 'Vector<T>' and 'Vector<T>.TangentVector' to be the same}}
// expected-error @+3 {{generic signature requires types 'Vector<U>' and 'Vector<U>.TangentVector' to be the same}}
// expected-error @+2 {{parameter type 'Vector<T>' does not conform to 'Differentiable' and satisfy 'Vector<T> == Vector<T>.TangentVector', but the enclosing function type is '@differentiable(linear)'}}
// expected-error @+1 {{result type 'Vector<U>' does not conform to 'Differentiable' and satisfy 'Vector<U> == Vector<U>.TangentVector', but the enclosing function type is '@differentiable(linear)'}}
func inferredConformancesGenericLinear<T, U>(_: @differentiable(linear) (Vector<T>) -> Vector<U>) {}

func nondiff(x: Vector<Int>) -> Vector<Int> {}
// expected-error @+1 {{global function 'inferredConformancesGeneric' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGeneric(nondiff)
// expected-error @+1 {{global function 'inferredConformancesGenericLinear' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGenericLinear(nondiff)

func diff(x: Vector<Float>) -> Vector<Float> {}
inferredConformancesGeneric(diff) // okay!

func inferredConformancesGenericResult<T, U>() -> @differentiable (Vector<T>) -> Vector<U> {}
// expected-error @+4 {{generic signature requires types 'Vector<T>' and 'Vector<T>.TangentVector' to be the same}}
// expected-error @+3 {{generic signature requires types 'Vector<U>' and 'Vector<U>.TangentVector' to be the same}}
// expected-error @+2 {{parameter type 'Vector<T>' does not conform to 'Differentiable' and satisfy 'Vector<T> == Vector<T>.TangentVector', but the enclosing function type is '@differentiable(linear)'}}
// expected-error @+1 {{result type 'Vector<U>' does not conform to 'Differentiable' and satisfy 'Vector<U> == Vector<U>.TangentVector', but the enclosing function type is '@differentiable(linear)'}}
func inferredConformancesGenericResultLinear<T, U>() -> @differentiable(linear) (Vector<T>) -> Vector<U> {}

struct Linear<T> {
  var x, y: T
}
extension Linear: Equatable where T: Equatable {}
extension Linear: AdditiveArithmetic where T: AdditiveArithmetic {}
extension Linear: Differentiable where T: Differentiable, T == T.TangentVector {
  typealias TangentVector = Self
}

// expected-note @+1 2 {{where 'T' = 'Int'}}
func inferredConformancesGeneric<T, U>(_: @differentiable (Linear<T>) -> Linear<U>) {}

// expected-note @+1 2 {{where 'T' = 'Int'}}
func inferredConformancesGenericLinear<T, U>(_: @differentiable(linear) (Linear<T>) -> Linear<U>) {}

func nondiff(x: Linear<Int>) -> Linear<Int> {}
// expected-error @+1 {{global function 'inferredConformancesGeneric' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGeneric(nondiff)
// expected-error @+1 {{global function 'inferredConformancesGenericLinear' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGenericLinear(nondiff)

func diff(x: Linear<Float>) -> Linear<Float> {}
inferredConformancesGeneric(diff) // okay!

func inferredConformancesGenericResult<T, U>() -> @differentiable (Linear<T>) -> Linear<U> {}
func inferredConformancesGenericResultLinear<T, U>() -> @differentiable(linear) (Linear<T>) -> Linear<U> {}
