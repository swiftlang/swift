// RUN: %target-swift-frontend -typecheck -verify %s

let _: @differentiable (Float) -> Float
let _: @differentiable (Float) throws -> Float

//
// Type differentiability
//

struct NonDiffType { var x: Int }
// FIXME: Properly type-check parameters and the result's differentiability
// expected-error @+1 {{argument is not differentiable, but the enclosing function type is marked '@differentiable'}} {{25-25=@nondiff }}
let _: @differentiable (NonDiffType) -> Float
// expected-error @+1 {{result is not differentiable, but the function type is marked '@differentiable'}}
let _: @differentiable (Float) -> NonDiffType

//
// Argument selection (@nondiff)
//

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

let nondiffVectorFunc: (Vector<Int>) -> Vector<Int>
// expected-error @+1 2 {{global function 'inferredConformancesGeneric' requires that 'Int' conform to 'Differentiable}}
inferredConformancesGeneric(nondiffVectorFunc)

let diffVectorFunc: (Vector<Float>) -> Vector<Float>
inferredConformancesGeneric(diffVectorFunc) // okay!

func inferredConformancesGenericResult<T, U>() -> @differentiable (Vector<T>) -> Vector<U> {}

//
// linear function type
//

// expected-error @+1 {{cannot mark functions as linear differentiable}}
let _: @differentiable(Float) (Float) -> Float
