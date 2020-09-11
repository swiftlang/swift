// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-typecheck-verify-swift

// Test usages of synthesized `Differentiable` member struct types.

// TF-466: Test conforming a synthesized member type to a protocol with
// property requirements.
protocol Proto {
  var weight: Float { get }
}
struct Foo : Differentiable {
  var weight: Float
}
// Note: the global variable here is necessary for type-checking to pass
// when extending a synthesized member type. Enabling general extensions of
// synthesized member types require extra non-trivial work, due to the
// current type-checker design.
let randomGlobal = 1
extension Foo.TangentVector : Proto {}
