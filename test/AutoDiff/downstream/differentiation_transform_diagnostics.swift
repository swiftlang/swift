// RUN: %target-swift-frontend -emit-sil -verify %s
// Negative test: verify that forward-mode differentiation crashes.
// RUN: not --crash %target-swift-frontend -enable-experimental-forward-mode-differentiation -emit-sil -verify %s

// This file tests SIL diagnostics during the differentiation transform.

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

func one_to_one_0(_ x: Float) -> Float {
  return x + 2
}

_ = gradient(at: 0, in: one_to_one_0) // okay!

//===----------------------------------------------------------------------===//
// Non-differentiable stored properties
//===----------------------------------------------------------------------===//

struct S {
  var p: Float
}
extension S : Differentiable, AdditiveArithmetic {
  // Test custom `TangentVector` type with non-matching stored property name.
  struct TangentVector: Differentiable, AdditiveArithmetic {
    var dp: Float
  }
  typealias AllDifferentiableVariables = S
  static var zero: S { return S(p: 0) }
  typealias Scalar = Float
  static func + (lhs: S, rhs: S) -> S { return S(p: lhs.p + rhs.p) }
  static func - (lhs: S, rhs: S) -> S { return S(p: lhs.p - rhs.p) }
  static func * (lhs: Float, rhs: S) -> S { return S(p: lhs * rhs.p) }

  mutating func move(along direction: TangentVector) {
    p.move(along: direction.dp)
  }
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+1 {{property cannot be differentiated because 'S.TangentVector' does not have a member named 'p'}}
_ = gradient(at: S(p: 0)) { s in 2 * s.p }

struct NoDerivativeProperty : Differentiable {
  var x: Float
  @noDerivative var y: Float
}
_ = gradient(at: NoDerivativeProperty(x: 1, y: 1)) { s -> Float in
  var tmp = s
  tmp.y = tmp.x // No diagnostics expected.
  return tmp.x
}
_ = gradient(at: NoDerivativeProperty(x: 1, y: 1)) { s in
  // TODO(TF-788): Re-enable non-varied result warning.
  // xpected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}} {{13-13=)}}
  return s.y
}
_ = gradient(at: NoDerivativeProperty(x: 1, y: 1)) {
  // TODO(TF-788): Re-enable non-varied result warning.
  // xpected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{3-3=withoutDerivative(at:}} {{7-7=)}}
  $0.y
}

//===----------------------------------------------------------------------===//
// Function composition
//===----------------------------------------------------------------------===//

func base(_ x: Float) -> Float {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
  return Float(Int(x))
}

// TODO: Fix nested differentiation diagnostics. Need to fix indirect differentiation invokers.
func nested(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return base(x)
}

func middle(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return nested(x)
}

func middle2(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return middle(x)
}

func func_to_diff(_ x: Float) -> Float {
  // xpected-note @+1 {{expression is not differentiable}}
  return middle2(x)
}

func calls_grad_of_nested(_ x: Float) -> Float {
  // xpected-error @+1 {{function is not differentiable}}
  return gradient(at: x, in: func_to_diff)
}

//===----------------------------------------------------------------------===//
// Enum differentiation
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func usesOptionals(_ x: Float) -> Float {
  // expected-note @+1 {{differentiating enum values is not yet supported}}
  var maybe: Float? = 10
  maybe = x
  return maybe!
}

enum DirectEnum: Differentiable & AdditiveArithmetic {
  case leaf(Float)

  typealias TangentVector = Self

  static var zero: Self { fatalError() }
  static func +(_ lhs: Self, _ rhs: Self) -> Self { fatalError() }
  static func -(_ lhs: Self, _ rhs: Self) -> Self { fatalError() }
}

// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: e)
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{differentiating enum values is not yet supported}}
func enum_active(_ e: DirectEnum, _ x: Float) -> Float {
  switch e {
  case let .leaf(y): return y
  }
}

// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: e)
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{differentiating enum values is not yet supported}}
func activeEnumValue(_ e: DirectEnum, _ x: Float) -> Float {
  switch e {
  case let .leaf(y): return y
  }
}

enum IndirectEnum<T: Differentiable>: Differentiable & AdditiveArithmetic {
  case leaf(T)

  typealias TangentVector = Self

  static func ==(_ lhs: Self, _ rhs: Self) -> Bool { fatalError() }
  static var zero: Self { fatalError() }
  static func +(_ lhs: Self, _ rhs: Self) -> Self { fatalError() }
  static func -(_ lhs: Self, _ rhs: Self) -> Self { fatalError() }
}

// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: e)
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{differentiating enum values is not yet supported}}
func activeEnumAddr(_ e: IndirectEnum<Float>, _ x: Float) -> Float {
  switch e {
  case let .leaf(y): return y
  }
}

//===----------------------------------------------------------------------===//
// Control flow
//===----------------------------------------------------------------------===//

func if_else(_ x: Float, _ flag: Bool) -> Float {
  let y: Float
  if flag {
    y = x + 1
  } else {
    y = x
  }
  return y
}

_ = gradient(at: 0) { x in if_else(x, true) }

//===----------------------------------------------------------------------===//
// @differentiable attributes
//===----------------------------------------------------------------------===//

var a: Float = 3.0
protocol P {
  @differentiable(wrt: x)
  func foo(x: Float) -> Float
}

enum T: P {
  // expected-error @+2 {{function is not differentiable}}
  // expected-note @+2 {{when differentiating this function definition}}
  @differentiable(wrt: x)
  func foo(x: Float) -> Float {
    // expected-note @+1 {{cannot differentiate writes to global variables}}
    a = a + x
    return a
  }
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable(wrt: x)
func foo(x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate writes to global variables}}
  a = a + x
  return a
}

// Test `@differentiable` on initializer with assignments.
struct TF_305 : Differentiable {
  var filter: Float
  var bias: Float
  typealias Activation = @differentiable (Float) -> Float
  @noDerivative let activation: Activation
  @noDerivative let strides: (Int, Int)

  @differentiable
  public init(
    filter: Float,
    bias: Float,
    activation: @escaping Activation,
    strides: (Int, Int)
  ) {
    self.filter = filter
    self.bias = bias
    self.activation = activation
    self.strides = strides
  }
}

// TF-676: Test differentiation of protocol requirement with multiple
// `@differentiable` attributes.
protocol MultipleDiffAttrsProto : Differentiable {
  @differentiable(wrt: (self, x))
  @differentiable(wrt: x)
  func f(_ x: Float) -> Float
}
func testMultipleDiffAttrsProto<P: MultipleDiffAttrsProto>(_ p: P, _ x: Float) {
  _ = gradient(at: p, x) { p, x in p.f(x) }
  _ = gradient(at: x) { x in p.f(x) }
}

// TF-676: Test differentiation of class method with multiple `@differentiable`
// attributes.
class MultipleDiffAttrsClass : Differentiable {
  @differentiable(wrt: (self, x))
  @differentiable(wrt: x)
  func f(_ x: Float) -> Float { x }
}
func testMultipleDiffAttrsClass<C: MultipleDiffAttrsClass>(_ c: C, _ x: Float) {
  _ = gradient(at: c, x) { c, x in c.f(x) }
  _ = gradient(at: x) { x in c.f(x) }
}

//===----------------------------------------------------------------------===//
// Classes
//===----------------------------------------------------------------------===//

class Foo : Differentiable {
  @differentiable
  func method(_ x: Float) -> Float {
    return x
  }

  // Not marked with `@differentiable`.
  func method2(_ x: Float) -> Float {
    return x
  }

  var nonDifferentiableStored: Float = 1

  @differentiable
  func testNonDifferentiableRefElementAddr(_ x: Float) -> Float {
    // expected-error @+2 {{expression is not differentiable}}
    // expected-note @+1 {{member is not differentiable because the corresponding class member is not '@differentiable'}}
    return nonDifferentiableStored * x
  }

  @differentiable
  var stored: Float = 1

  @differentiable
  func testRefElementAddr(_ x: Float) -> Float {
    return stored * x
  }
}

@differentiable
func differentiateClassMethod(x: Float) -> Float {
  return Foo().method(x)
}

@differentiable
func differentiateClassMethod2(x: Float) -> Float {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{member is not differentiable because the corresponding class member is not '@differentiable'}}
  return Foo().method2(x)
}

let _: @differentiable (Float) -> Float = Foo().method
_ = gradient(at: .zero, in: Foo().method)

// TF-1149: Test class with loadable type but address-only `TangentVector` type.
class C<T: Differentiable>: Differentiable {
  // expected-error @+1 {{function is not differentiable}}
  @differentiable
  // expected-note @+2 {{when differentiating this function definition}}
  // expected-note @+1 {{cannot yet differentiate value whose type 'C<T>' has a compile-time known size, but whose 'TangentVector' contains stored properties of unknown size; consider modifying 'C<τ_0_0>.TangentVector' to use fewer generic parameters in stored properties}}
  var stored: T

  init(_ stored: T) {
    self.stored = stored
  }

  // expected-error @+1 {{function is not differentiable}}
  @differentiable
  // expected-note @+2 {{when differentiating this function definition}}
  // expected-note @+1 {{cannot yet differentiate value whose type 'C<T>' has a compile-time known size, but whose 'TangentVector' contains stored properties of unknown size; consider modifying 'C<τ_0_0>.TangentVector' to use fewer generic parameters in stored properties}}
  func foo(_ x: T) -> T {
    stored
  }
}

//===----------------------------------------------------------------------===//
// Unreachable
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
let no_return: @differentiable (Float) -> Float = { x in
  let _ = x + 1
// expected-error @+2 {{missing return in a closure expected to return 'Float'}}
// expected-note @+1 {{missing return for differentiation}}
}

//===----------------------------------------------------------------------===//
// Multiple results
//===----------------------------------------------------------------------===//

func multipleResults(_ x: Float) -> (Float, Float) {
  return (x, x)
}
// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func usesMultipleResults(_ x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate through multiple results}}
  let tuple = multipleResults(x)
  return tuple.0 + tuple.1
}

//===----------------------------------------------------------------------===//
// Non-differentiable arguments and results
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func roundingGivesError(x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
  return Float(Int(x))
}

//===----------------------------------------------------------------------===//
// Inout arguments
//===----------------------------------------------------------------------===//

@differentiable
func activeInoutArg(_ x: Float) -> Float {
  var result = x
  result += x
  return result
}

@differentiable
func activeInoutArgNonactiveInitialResult(_ x: Float) -> Float {
  var result: Float = 1
  result += x
  return result
}

@differentiable
func activeInoutArgTuple(_ x: Float) -> Float {
  var tuple = (x, x)
  tuple.0 *= x
  return x * tuple.0
}

@differentiable
func activeInoutArgControlFlow(_ array: [Float]) -> Float {
  var result: Float = 1
  for i in withoutDerivative(at: array).indices {
    result += array[i]
  }
  return result
}

@differentiable
func activeInoutArgControlFlowComplex(_ array: [Float], _ bool: Bool) -> Float {
  var result: Float = 1
  if bool {
    if bool {}
    for i in withoutDerivative(at: array).indices {
      switch i % 2 {
      case 0: continue
      case 1: break
      default: break
      }
      result = result + 1
      result += array[i]
    }
  }
  return result
}

struct Mut: Differentiable {}
extension Mut {
  @differentiable(wrt: x)
  mutating func mutatingMethod(_ x: Mut) {}
}

@differentiable(wrt: x)
func nonActiveInoutArg(_ nonactive: inout Mut, _ x: Mut) {
  nonactive.mutatingMethod(x)
}

@differentiable(wrt: x)
func activeInoutArgMutatingMethod(_ x: Mut) -> Mut {
  var result = x
  result.mutatingMethod(result)
  return result
}

@differentiable(wrt: x)
func activeInoutArgMutatingMethodVar(_ nonactive: inout Mut, _ x: Mut) {
  var result = nonactive
  result.mutatingMethod(x)
  nonactive = result
}

@differentiable(wrt: x)
func activeInoutArgMutatingMethodTuple(_ nonactive: inout Mut, _ x: Mut) {
  var result = (nonactive, x)
  result.0.mutatingMethod(result.0)
  nonactive = result.0
}

func twoInoutParameters(_ x: inout Float, _ y: inout Float) {}
// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func testTwoInoutParameters(_ x: Float, _ y: Float) -> Float {
  var x = x
  var y = y
  // expected-note @+1 {{cannot differentiate through multiple results}}
  twoInoutParameters(&x, &y)
  return x
}

func inoutParameterAndFormalResult(_ x: inout Float) -> Float { x }
// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func testInoutParameterAndFormalResult(_ x: Float) -> Float {
  var x = x
  // expected-note @+1 {{cannot differentiate through multiple results}}
  return inoutParameterAndFormalResult(&x)
}

//===----------------------------------------------------------------------===//
// Non-varied results
//===----------------------------------------------------------------------===//

func one() -> Float {
  return 1
}
@differentiable
func nonVariedResult(_ x: Float) -> Float {
  // TODO(TF-788): Re-enable non-varied result warning.
  // xpected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}} {{15-15=)}}
  return one()
}

// Check that `withoutDerivative(at:)` silences the warning.

struct TF_775: Differentiable {
  @differentiable(wrt: (self))
  func nonVariedResult(_ input: Float) -> Float {
    withoutDerivative(at: input)
  }
}

//===----------------------------------------------------------------------===//
// Tuple differentiability
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func tupleArrayLiteralInitialization(_ x: Float, _ y: Float) -> Float {
  // `Array<(Float, Float)>` does not conform to `Differentiable`.
  let array = [(x * y, x * y)]
  // expected-note @+1 {{cannot differentiate through a non-differentiable argument; do you want to use 'withoutDerivative(at:)'?}}
  return array[0].0
}

//===----------------------------------------------------------------------===//
// Subset parameters
//===----------------------------------------------------------------------===//

func nondiff(_ f: @differentiable (Float, @noDerivative Float) -> Float) -> Float {
  // expected-note @+2 {{cannot differentiate with respect to a '@noDerivative' parameter}}
  // expected-error @+1 {{function is not differentiable}}
  return gradient(at: 2) { x in f(x * x, x) }
}

// Test parameter subset thunk + partially-applied original function.
struct TF_675 : Differentiable {
  @differentiable
  func method(_ x: Float) -> Float {
    return x
  }
}
let _: @differentiable (Float) -> Float = TF_675().method

// TF-918: test parameter subset thunk + partially-applied original function.
_ = gradient(at: Float(1), Float(2), in: (+) as @differentiable (Float, @noDerivative Float) -> Float)

//===----------------------------------------------------------------------===//
// Coroutines (SIL function yields, `begin_apply`) (not yet supported)
//===----------------------------------------------------------------------===//

struct HasCoroutineAccessors: Differentiable {
  var stored: Float
  var computed: Float {
    // `_read` is a coroutine: `(Self) -> () -> ()`.
    _read { yield stored }
    // `_modify` is a coroutine: `(inout Self) -> () -> ()`.
    _modify { yield &stored }
  }
}
// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func testAccessorCoroutines(_ x: HasCoroutineAccessors) -> HasCoroutineAccessors {
  var x = x
  // expected-note @+1 {{differentiation of coroutine calls is not yet supported}}
  x.computed = x.computed
  return x
}

// TF-1078: diagnose `_modify` accessor application with active `inout` argument.
// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func TF_1078(array: [Float], x: Float) -> Float {
  var array = array
  // Array subscript assignment below calls `Array.subscript.modify`.
  // expected-note @+1 {{differentiation of coroutine calls is not yet supported}}
  array[0] = x
  return array[0]
}

// TF-1115: diagnose `_modify` accessor application with initially non-active `inout` argument.
// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func TF_1115(_ x: Float) -> Float {
  var array: [Float] = [0]
  // Array subscript assignment below calls `Array.subscript.modify`.
  // expected-note @+1 {{differentiation of coroutine calls is not yet supported}}
  array[0] = x
  return array[0]
}

// TF-1115: test `_modify` accessor application with initially non-active `inout` argument,
// where the yielded value is not a projection from `self`.
var global: Float = 1
extension Float {
  var projection: Float {
    get { self }
    // This `modify` accessor yields a global variable, not a projection from `self`.
    // Diagnosing active applications is nonetheless a safe over-approximation.
    _modify { yield &global }
  }
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func TF_1115_modifyNonSelfProjection(x: Float) -> Float {
  var result: Float = 0
  // Assignment below calls `Float.projection.modify`.
  // expected-note @+1 {{differentiation of coroutine calls is not yet supported}}
  result.projection = x
  return result
}

//===----------------------------------------------------------------------===//
// Conversion to `@differentiable(linear)` (not yet supported)
//===----------------------------------------------------------------------===//

// expected-error @+1 {{conversion to '@differentiable(linear)' function type is not yet supported}}
let _: @differentiable(linear) (Float) -> Float = { x in x }

//===----------------------------------------------------------------------===//
// Differentiating from fragile functions
//===----------------------------------------------------------------------===//

public func implicitlyDifferentiableFromFragile(_ x: Float) -> Float { x }

public func hasImplicitlyDifferentiatedTopLevelDefaultArgument(
// expected-error @+2 {{function is not differentiable}}
// expected-note @+1 {{differentiated functions in default arguments must be marked '@differentiable' or have a public '@derivative'}}
  _ f: @differentiable (Float) -> Float = implicitlyDifferentiableFromFragile
) {}

// TODO(TF-1030): This will eventually not be an error.
// expected-error @+2 {{function is not differentiable}}
// expected-note @+1 {{differentiated functions in default arguments must be marked '@differentiable' or have a public '@derivative'; this is not possible with a closure, make a top-level function instead}}
public func hasImplicitlyDifferentiatedClosureDefaultArgument(_ f: @differentiable (Float) -> Float = { $0 }) {}

@inlinable
public func fragileFuncWithGradient() {
  // expected-error @+2 {{function is not differentiable}}
  // expected-note @+1 {{differentiated functions in '@inlinable' functions must be marked '@differentiable' or have a public '@derivative'}}
  let _ = gradient(at: 0, in: implicitlyDifferentiableFromFragile)
}

@inlinable
@differentiable
public func fragileDifferentiable(_ x: Float) -> Float {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{differentiated functions in '@inlinable' functions must be marked '@differentiable' or have a public '@derivative'}}
  implicitlyDifferentiableFromFragile(x)
}

// TF-1208: Test curry thunk differentiation regression.
public struct TF_1208_Struct<Scalar> {
  var x: Scalar
}
extension TF_1208_Struct: Differentiable where Scalar: Differentiable {
  @differentiable
  public static func id(x: Self) -> Self {
    return x
  }
}
@differentiable(wrt: x)
public func TF_1208<Scalar: Differentiable>(
  _ x: TF_1208_Struct<Scalar>,
  // NOTE(TF-1208): This diagnostic is unexpected because `TF_1208_Struct.id` is marked `@differentiable`.
  // expected-error @+2 {{function is not differentiable}}
  // expected-note @+1 {{differentiated functions in '@inlinable' functions must be marked '@differentiable' or have a public '@derivative'; this is not possible with a closure, make a top-level function instead}}
  reduction: @differentiable (TF_1208_Struct<Scalar>) -> TF_1208_Struct<Scalar> = TF_1208_Struct.id
) -> TF_1208_Struct<Scalar> {
  reduction(x)
}
