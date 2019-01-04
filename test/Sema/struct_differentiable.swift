// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify %s -verify-ignore-unknown

struct Simple : VectorNumeric, Differentiable {
  var w: Float
  var b: Float
}
var simple = Simple(w: 1, b: 1)
assert(simple.moved(along: simple) == simple + simple)
assert(simple.tangentVector(from: simple) == simple)

// Test type with mixed members.
struct Mixed : VectorNumeric, Differentiable {
  var simple: Simple
  var float: Float
}
var mixed = Mixed(simple: simple, float: 1)
assert(mixed.moved(along: mixed) == mixed + mixed)
assert(mixed.tangentVector(from: mixed) == mixed)

// Test type with generic members that conform to `Differentiable`.
// Since `T == T.TangentVector == T.CotangentVector`,
// it's only necessary to synthesis typealiases:
// `typealias TangentVector = Generic`
// `typealias CotangentVector = Generic`
struct Generic<T : VectorNumeric> : VectorNumeric, Differentiable
  where T : Differentiable, T == T.TangentVector, T == T.CotangentVector
{
  var w: T
  var b: T
}
var generic = Generic<Double>(w: 1, b: 1)
assert(generic.moved(along: generic) == generic + generic)
assert(generic.tangentVector(from: generic) == generic)

// Test type with manual definition of vector space types to `Self`.
struct VectorSpacesEqualSelf : VectorNumeric, Differentiable {
  var w: Float
  var b: Float
  typealias TangentVector = VectorSpacesEqualSelf
  typealias CotangentVector = VectorSpacesEqualSelf
}

// TODO: Support the cases below after `Differentiable` derived conformances
// limitations are lifted.

/*
// Test type with generic members that conform to `Differentiable`.
// Since it's not the case that
// `T == T.TangentVector == T.CotangentVector`,
// it's necessary to synthesize new vector space struct types.
struct GenericNeedsVectorSpaceStructs<T> : VectorNumeric, Differentiable
  where T : VectorNumeric, T : Differentiable
{
  var w: T
  var b: T
}

// Test type that doesn't conform to `VectorNumeric`.
// Thus, `Self` cannot be used as `TangentVector` or `CotangentVector`.
// Vector space structs types must be synthesized.
// Note: it would be nice to emit a warning if conforming `Self` to
// `VectorNumeric` is possible.
struct NotVectorNumeric : Differentiable {
  var w: Float
  var b: Float
}
*/

// Test type that doesn't conform to `VectorNumeric`.
// Currently, vector space type synthesis is not possible.
// TODO: Replace this test with above commented test once the vector space
// synthesis restriction is lifted.
struct NotVectorNumeric : Differentiable { // expected-error {{type 'NotVectorNumeric' does not conform to protocol 'Differentiable'}}
  var w: Float
  var b: Float
}

// Test errors.

// Test manually customizing vector space types.
// Thees should fail. Synthesis is semantically unsupported if vector space
// types are customized.
struct VectorSpaceTypeAlias : VectorNumeric, Differentiable { // expected-error {{type 'VectorSpaceTypeAlias' does not conform to protocol 'Differentiable'}}
  var w: Float
  var b: Float
  typealias TangentVector = Simple
}
struct VectorSpaceCustomStruct : VectorNumeric, Differentiable { // expected-error {{type 'VectorSpaceCustomStruct' does not conform to protocol 'Differentiable'}}
  var w: Float
  var b: Float
  struct CotangentVector : VectorNumeric, Differentiable {
    var w: Float.CotangentVector
    var b: Float.CotangentVector
    typealias TangentVector = VectorSpaceCustomStruct.CotangentVector
    typealias CotangentVector = VectorSpaceCustomStruct.CotangentVector
  }
}


// Test type whose properties are not all differentiable.
struct DifferentiableSubset : Differentiable {
  @differentiable(wrt: (self))
  var w: Float
  @differentiable(wrt: (self))
  var b: Float
  @noDerivative var flag: Bool
 
  @_fieldwiseProductSpace
  struct TangentVector : Differentiable, VectorNumeric {
    @_fieldwiseProductSpace
    typealias TangentVector = DifferentiableSubset.TangentVector
    @_fieldwiseProductSpace
    typealias CotangentVector = DifferentiableSubset.CotangentVector
    var w: Float
    var b: Float
    func tangentVector(from cotan: CotangentVector) -> TangentVector {
      return TangentVector(w: cotan.w, b: cotan.b)
    }
  }
  @_fieldwiseProductSpace
  struct CotangentVector : Differentiable, VectorNumeric {
    @_fieldwiseProductSpace
    typealias TangentVector = DifferentiableSubset.CotangentVector
    @_fieldwiseProductSpace
    typealias CotangentVector = DifferentiableSubset.TangentVector
    var w: Float
    var b: Float
    func tangentVector(from cotan: CotangentVector) -> TangentVector {
      return TangentVector(w: cotan.w, b: cotan.b)
    }
  }
  func tangentVector(from cotan: CotangentVector) -> TangentVector {
    return TangentVector(w: cotan.w, b: cotan.b)
  }
  func moved(along v: TangentVector) -> DifferentiableSubset {
    return DifferentiableSubset(w: w.moved(along: v.w), b: b.moved(along: v.b), flag: flag)
  }
}
