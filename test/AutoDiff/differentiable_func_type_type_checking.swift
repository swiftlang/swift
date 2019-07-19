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

// expected-error @+1 {{'@differentiable(linear)' types are not yet supported}}
let _: @differentiable(linear) (Float) -> Float

//===----------------------------------------------------------------------===//
// Function conversion
//===----------------------------------------------------------------------===//

func nonescapingArgument(f: @differentiable (Float, Float) -> Float) -> Float {
  return gradient(at: 1) { x in f(x, x) }
}

func nonescapingArgumentError(
  f: @differentiable (Float, Float) -> Float
) -> @differentiable (Float) -> Float{
  return { x in f(x, x) }
}

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
// expected-error @+2 {{result is not differentiable, but the function type is marked '@differentiable'}}
// expected-error @+1 {{result is not differentiable, but the function type is marked '@differentiable'}}
func test3<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> @differentiable (U) -> Int) {}
// expected-error @+1 {{result is not differentiable, but the function type is marked '@differentiable'}}
func test4<T: Differentiable, U: Differentiable>(_: @differentiable (T) -> (U) -> Int) {}

let diffFunc: @differentiable (Float) -> Float
func inferredConformances<T, U>(_: @differentiable (T) -> U) {}
inferredConformances(diffFunc)

func inferredConformancesResult<T, U>() -> @differentiable (T) -> U {}

let diffFuncWithNondiff: @differentiable (Float, @nondiff Int) -> Float
func inferredConformances<T, U, V>(_: @differentiable (T, @nondiff U) -> V) {}
inferredConformances(diffFuncWithNondiff)

struct Vector<T> {
  var x, y: T
}
extension Vector: Differentiable where T: Differentiable {}

// expected-note @+2 {{where 'T' = 'Int'}}
// expected-note @+1 {{where 'U' = 'Int'}}
func inferredConformancesGeneric<T, U>(_: @differentiable (Vector<T>) -> Vector<U>) {}

func nondiffVectorFunc(x: Vector<Int>) -> Vector<Int> {}
// expected-error @+1 2 {{global function 'inferredConformancesGeneric' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGeneric(nondiffVectorFunc)

func diffVectorFunc(x: Vector<Float>) -> Vector<Float> {}
inferredConformancesGeneric(diffVectorFunc) // okay!

func inferredConformancesGenericResult<T, U>() -> @differentiable (Vector<T>) -> Vector<U> {}
