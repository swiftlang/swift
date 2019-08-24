// RUN: %target-typecheck-verify-swift

@_alignment(16)
struct Float4 {
  var x, y, z, w: Float
}
@_alignment(1)
struct AnyAlignment {}
@_alignment(1)
enum AnyAlignmentEnum {}


@_alignment // expected-error{{expected '('}}
struct DidntSpecifyAlignment {}

@_alignment() // expected-error{{must be a positive integer literal}} expected-error{{expected declaration}}
struct DidntSpecifyAlignment2 {}

@_alignment("sixteen") // expected-error{{must be a positive integer literal}} expected-error{{expected declaration}}
struct SpecifiedJunkAlignment {}

@_alignment(-1) // expected-error{{must be a positive integer literal}} expected-error{{expected declaration}}
struct NegativeAlignment {}

@_alignment(0) // expected-error{{must be a power of two}}
struct ZeroAlignment {}

@_alignment(3) // expected-error{{must be a power of two}}
struct NonPowerOfTwoAlignment {}

@_alignment(3.5) // expected-error{{must be a positive integer literal}} expected-error{{expected declaration}}
struct FractionalAlignment {}

@_alignment(16) // expected-error{{cannot be applied to this declaration}} {{1-17=}}
class ClassWithAlignment {}

@_alignment(16) // expected-error{{cannot be applied to this declaration}} {{1-17=}}
protocol ProtocolWithAlignment {}

struct Foo {
  @_alignment(16) var fieldWithAlignment: Int // expected-error{{cannot be applied to this declaration}} {{3-19=}}
  @_alignment(16) func funcWithAlignment() {} // expected-error{{cannot be applied to this declaration}} {{3-19=}}
}
