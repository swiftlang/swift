// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify %s -verify-ignore-unknown

struct Simple : VectorNumeric, Differentiable {
  var w: Float
  var b: Float
}
var x = Simple(w: 1, b: 1)
assert(x.moved(along: x) == x + x)
assert(x.tangentVector(from: x) == x)

// Test type with mixed members.
struct Mixed : VectorNumeric, Differentiable {
  var tensor: Tensor<Float>
  var float: Float
}
var mixed = Mixed(tensor: Tensor(1), float: 1)
assert(mixed.moved(along: mixed) == mixed + mixed)
assert(mixed.tangentVector(from: mixed) == mixed)

// Test type with generic members that conform to `Differentiable`.
// Since `Member == Member.TangentVector == Member.CotangentVector`,
// it's only necessary to synthesis typealiases:
// `typealias TangentVector = Generic`
// `typealias CotangentVector = Generic`
struct Generic<Member> : VectorNumeric, Differentiable
  where T : Differentiable, T == T.TangentVector, T == T.CotangentVector
{
  var w: T
  var b: T
}
var generic = Generic<Double>(w: 1, b: 1)
assert(generic.moved(along: generic) == generic + generic)
assert(generic.tangentVector(from: generic) == generic)

// TODO: Support the cases below after `Differentiable` derived conformances
// limitations are lifted.

/*
// Test type with generic members that conform to `Differentiable`.
// Since it's not the case that
// `Member == Member.TangentVector == Member.CotangentVector`,
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
