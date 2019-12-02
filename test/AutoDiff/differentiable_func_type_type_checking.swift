// RUN: %target-swift-frontend -typecheck -verify %s

let _: @differentiable (Float) -> Float
let _: @differentiable (Float) throws -> Float

//===----------------------------------------------------------------------===//
// Type differentiability
//===----------------------------------------------------------------------===//

struct NonDiffType { var x: Int }
// FIXME: Properly type-check parameters and the result's differentiability
// expected-error @+1 {{argument is not differentiable, but the enclosing function type is marked '@differentiable'}} {{25-25=@nondiff }}
let _: @differentiable (NonDiffType) -> Float
// expected-error @+1 {{result is not differentiable, but the function type is marked '@differentiable'}}
let _: @differentiable (Float) -> NonDiffType

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
// Parameter selection (@nondiff)
//===----------------------------------------------------------------------===//

// expected-error @+1 {{'nondiff' cannot be applied to arguments of a non-differentiable function}}
let _: (@nondiff Float, Float) -> Float

let _: @differentiable (Float, @nondiff Float) -> Float // okay

func foo<T: Differentiable, U: Differentiable>(x: T) -> U {
  let fn: (@differentiable (T) -> U)? = nil
  return fn!(x)
}

func test1<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> @differentiable (U) -> Float) {}
func test2<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> (U) -> Float) {}
// expected-error @+1 {{result is not differentiable, but the function type is marked '@differentiable'}}
func test3<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> @differentiable (U) -> Int) {}
func test4<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> (U) -> Int) {}

let diffFunc: @differentiable (Float) -> Float
let linearFunc: @differentiable(linear) (Float) -> Float
func inferredConformances<T, U>(_: @differentiable (T) -> U) {}
func inferredConformancesLinear<T, U>(_: @differentiable(linear) (T) -> U) {}
inferredConformances(diffFunc)
inferredConformancesLinear(linearFunc)

func inferredConformancesResult<T, U>() -> @differentiable (T) -> U {}
func inferredConformancesResultLinear<T, U>() -> @differentiable(linear) (T) -> U {}

let diffFuncWithNondiff: @differentiable (Float, @nondiff Int) -> Float
let linearFuncWithNondiff: @differentiable(linear) (Float, @nondiff Int) -> Float
func inferredConformances<T, U, V>(_: @differentiable (T, @nondiff U) -> V) {}
func inferredConformancesLinear<T, U, V>(_: @differentiable(linear) (T, @nondiff U) -> V) {}
inferredConformances(diffFuncWithNondiff)
inferredConformancesLinear(linearFuncWithNondiff)

struct Vector<T> {
  var x, y: T
}
extension Vector: Equatable where T: Equatable {}
extension Vector: Differentiable where T: Differentiable {}
extension Vector: AdditiveArithmetic where T: AdditiveArithmetic {}

// expected-note @+1 {{where 'T' = 'Int'}}
func inferredConformancesGeneric<T, U>(_: @differentiable (Vector<T>) -> Vector<U>) {}

// expected-note @+1 {{where 'T' = 'Int'}}
func inferredConformancesGenericLinear<T, U>(_: @differentiable(linear) (Vector<T>) -> Vector<U>) {}

func nondiffVectorFunc(x: Vector<Int>) -> Vector<Int> {}
// expected-error @+1 {{global function 'inferredConformancesGeneric' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGeneric(nondiffVectorFunc)
// expected-error @+1 {{global function 'inferredConformancesGenericLinear' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGenericLinear(nondiffVectorFunc)

func diffVectorFunc(x: Vector<Float>) -> Vector<Float> {}
inferredConformancesGeneric(diffVectorFunc) // okay!

func inferredConformancesGenericResult<T, U>() -> @differentiable (Vector<T>) -> Vector<U> {}
func inferredConformancesGenericResultLinear<T, U>() -> @differentiable(linear) (Vector<T>) -> Vector<U> {}
