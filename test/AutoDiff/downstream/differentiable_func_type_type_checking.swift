// RUN: %target-swift-frontend -typecheck -verify %s

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

//===----------------------------------------------------------------------===//
// Function conversion
//===----------------------------------------------------------------------===//

func takesOpaqueClosure(f: @escaping (Float) -> Float) {
  // expected-note @-1 {{did you mean to take a '@differentiable' closure?}} {{38-38=@differentiable }}
  // expected-error @+1 {{a '@differentiable' function can only be formed from a reference to a 'func' or a literal closure}}
  _ = gradient(of: f)
}

let globalAddOne: (Float) -> Float = { $0 + 1 }
// expected-error @+1 {{a '@differentiable' function can only be formed from a reference to a 'func' or a literal closure}}
_ = gradient(of: globalAddOne)

func someScope() {
  let localAddOne: (Float) -> Float = { $0 + 1 }
  // expected-error @+1 {{a '@differentiable' function can only be formed from a reference to a 'func' or a literal closure}}
  _ = gradient(of: globalAddOne)
  // expected-error @+1 {{a '@differentiable' function can only be formed from a reference to a 'func' or a literal closure}}
  _ = gradient(of: localAddOne)
  // The following case is okay during type checking, but will fail in the AD transform.
  _ = gradient { localAddOne($0) }
}

func addOne(x: Float) -> Float { x + 1 }
_ = gradient(of: addOne) // okay

extension Float {
  static func addOne(x: Float) -> Float { x + 1 }
  func addOne(x: Float) -> Float { x + 1 }
}
_ = gradient(of: Float.addOne) // okay
_ = gradient(of: Float(1.0).addOne) // okay

// TODO(TF-908): Remove this test once linear-to-differentiable conversion is supported.
func linearToDifferentiable(_ f: @escaping @differentiable(linear) (Float) -> Float) {
  // expected-error @+1 {{conversion from '@differentiable(linear)' to '@differentiable' is not yet supported}}
  _ = f as @differentiable (Float) -> Float
}

func differentiableToLinear(_ f: @escaping @differentiable (Float) -> Float) {
  // expected-error @+1 {{a '@differentiable(linear)' function can only be formed from a reference to a 'func' or a literal closure}}
  _ = f as @differentiable(linear) (Float) -> Float
}

//===----------------------------------------------------------------------===//
// Parameter selection (@noDerivative)
//===----------------------------------------------------------------------===//

// expected-warning @+1 {{'@nondiff' is deprecated; use '@noDerivative' instead}}
let _: @differentiable (@nondiff Float, Float) -> Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: (@noDerivative Float, Float) -> Float

let _: @differentiable (Float, @noDerivative Float) -> Float // okay

func foo<T: Differentiable, U: Differentiable>(x: T) -> U {
  let fn: (@differentiable (T) -> U)? = nil
  return fn!(x)
}
// expected-error @+1 {{result type '@differentiable (U) -> Float' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test1<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> @differentiable (U) -> Float) {}
// expected-error @+1 {{result type '(U) -> Float' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test2<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> (U) -> Float) {}
// expected-error @+2 {{result type 'Int' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
// expected-error @+1 {{result type '@differentiable (U) -> Int' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test3<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> @differentiable (U) -> Int) {}
// expected-error @+1 {{result type '(U) -> Int' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func test4<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> (U) -> Int) {}

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
extension Vector: AdditiveArithmetic where T: AdditiveArithmetic {}
extension Vector: Differentiable where T: Differentiable {}

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
