// RUN: %target-typecheck-verify-swift -swift-version 5

// Tests for the diagnostics emitted in Sema for checking constantness of
// function arguments annotated to be so. Note that these annotation are
// specific to the new os log overlay and the low-level atomics library.

// The following tests check different types of constants that are accepted
// by the constantness Sema check. It creates helper functions with the
// semnatics annotation: "oslog.requires_constant_arguments" which requires that
// all arguments passed to the function are constants. The annotation is meant
// to be used only by the os log overlay. The test helpers use it here only for
// the purpose of testing the functionality.

// Check simple literals.
@_semantics("oslog.requires_constant_arguments")
func constantArgumentFunction<T>(_ constArg: T) {
}

func literalTest(x: Int) {
  constantArgumentFunction(1)
  constantArgumentFunction("Some string")
  constantArgumentFunction(1.9)
  constantArgumentFunction(true)
  constantArgumentFunction(x)
    // expected-error@-1 {{argument must be an integer literal}}
  constantArgumentFunction(x + 2)
    // expected-error@-1 {{argument must be an integer literal}}
}

@_semantics("oslog.requires_constant_arguments")
func constArgFunctionWithReturn<T>(_ constArg: T) -> T {
  return constArg
}

func testConstArgFuncWithReturn(x: Int, str: String) -> Int {
  _ = constArgFunctionWithReturn("")
  _ = constArgFunctionWithReturn(str)
    // expected-error@-1 {{argument must be a string literal}}
  constArgFunctionWithReturn(10)
    // expected-warning@-1 {{result of call to 'constArgFunctionWithReturn' is unused}}
  return constArgFunctionWithReturn(x)
    // expected-error@-1 {{argument must be an integer literal}}
}

@_semantics("oslog.requires_constant_arguments")
func constantOptionalArgument(_ constArg: Optional<Int>) {
}

// Correct test cases.
func optionalTest(x: Int) {
  constantOptionalArgument(nil)
  constantOptionalArgument(0)
  constantArgumentFunction(x + 2)
    // expected-error@-1 {{argument must be an integer literal}}
}

// Test string interpolation literals. We can only enforce constantness on custom string
// interpolation types. For string types, the constant is a string literal.

struct CustomStringInterpolation : ExpressibleByStringLiteral,
  ExpressibleByStringInterpolation {
  struct StringInterpolation : StringInterpolationProtocol {
    init(literalCapacity: Int, interpolationCount: Int) { }
    mutating func appendLiteral(_ x: String) { }

    @_semantics("oslog.requires_constant_arguments")
    mutating func appendInterpolation(_ x: Int) { }
  }
  init(stringLiteral value: String) { }
  init(stringInterpolation: StringInterpolation) { }
}

@_semantics("oslog.requires_constant_arguments")
func constantStringInterpolation(_ constArg: CustomStringInterpolation) {}

func testStringInterpolationLiteral(x: Int) {
  constantStringInterpolation("a string interpolation literal \(x)")
    // expected-error@-1 {{argument must be an integer literal}}
  constantStringInterpolation("a string interpolation literal \(10)")
}

// Test multiple arguments.
@_semantics("oslog.requires_constant_arguments")
func multipleArguments(_ arg1: Int, _ arg2: Bool, _ arg3: String, _ arg4: Double) {
}

func testMultipleArguments(_ x: String, _ y: Double) {
  multipleArguments(56, false, "", 23.3)
  multipleArguments(56, false, x, y)
    // expected-error@-1 {{argument must be a string literal}}
    // expected-error@-2 {{argument must be a floating-point literal}}
}

// Test enum uses.
enum Color {
  case red
  case blue
  case green
  case rgb(r: Int, g: Int, b: Int)
}

@_semantics("oslog.requires_constant_arguments")
func enumArgument(_ color: Color) { }

func testEnumArgument(r: Int, c: Color) {
  enumArgument(.rgb(r: 12, g: 0, b: 1))
  enumArgument(.green)
  enumArgument(.rgb(r: r, g: 200, b: 453))
    // expected-error@-1 {{argument must be an integer literal}}
  enumArgument(c)
    // expected-error@-1 {{argument must be a case of enum 'Color'}}
}

// Test type expressions.
@_semantics("oslog.requires_constant_arguments")
func typeArgument<T>(_ t: T.Type) { }

func testTypeArgument<S>(_ t: S.Type) {
  typeArgument(Int.self)
  typeArgument(S.self)
  typeArgument(t)
   // expected-error@-1 {{argument must be a <Type>.self}}
}

// Test constant evaluable function calls.
@_semantics("constant_evaluable")
func constantEval(_ x: Int, _ y: Bool) -> Int { x + 100 }

func testConstantEvalArgument(x: Int) {
  constantArgumentFunction(constantEval(90, true))
  constantArgumentFunction(constantEval(constantEval(500, true), false))
  constantArgumentFunction(constantEval(x, true))
    // expected-error@-1 {{argument must be an integer literal}}
}

// Test constant evaluable function calls with default arguments.
@_semantics("constant_evaluable")
func constantEvalAdvanced(_ x: () -> Int, _ y: Bool = true, z: String) { }

func testConstantEvalAdvanced(arg: Int) {
  constantArgumentFunction(constantEvalAdvanced({ arg }, z: ""))
}

// Test constant evaluable methods.
struct E  {
  // expected-note@-1 {{'E' declared here}}
  func constantEvalMethod1() -> E { return self }

  @_semantics("constant_evaluable")
  func constantEvalMethod2() -> E { return self }

  @_semantics("constant_evaluable")
  static func constantEvalMethod3(x: Bool) -> E { return E() }

  @_semantics("constant_evaluable")
  static func constantEvalMethod4() -> E { return E() }
}

@_semantics("oslog.requires_constant_arguments")
func functionNeedingConstE(_ x: E) { }

func testConstantEvalMethod(b: Bool) {
  functionNeedingConstE(E().constantEvalMethod1())
    // expected-error@-1 {{argument must be a static method or property of 'E'}}
  functionNeedingConstE(E().constantEvalMethod2())
  functionNeedingConstE(.constantEvalMethod3(x: true))
  functionNeedingConstE(.constantEvalMethod3(x: b))
    // expected-error@-1 {{argument must be a bool literal}}
  functionNeedingConstE(.constantEvalMethod4())
}

// Test functions with autoclosures.
@_semantics("oslog.requires_constant_arguments")
func autoClosureArgument(_ number: @autoclosure @escaping () -> Int) { }

func testAutoClosure(_ number: Int) {
  autoClosureArgument(number)
}

@_semantics("constant_evaluable")
func constEvalWithAutoClosure(_ number: @autoclosure @escaping () -> Int) -> Int {
  return 0
}

func testConstantEvalAutoClosure(_ number: Int) {
  constantArgumentFunction(constEvalWithAutoClosure(number))
}

// Test nested use of constant parameter.
@_semantics("oslog.requires_constant_arguments")
func testConstantArgumentRequirementPropagation(constParam: Int) {
  constantArgumentFunction(constParam)
}

// Test nested use of constant parameter in constant evaluable function.
@_semantics("oslog.requires_constant_arguments")
func testConstantArgumentWithConstEval(constParam: Int) {
  constantArgumentFunction(constantEval(constParam, true))
}

// Test parital-apply of constantArgumentFunction.
@_semantics("oslog.requires_constant_arguments")
func constArg2(_ x: Int) -> Int { x }

// This is not an error.
func testPartialApply() -> ((Int) -> Int) {
  return constArg2
}

@_semantics("oslog.requires_constant_arguments")
func constArg3(_ x: (Int) -> Int) -> Int { x(0) }

@_semantics("constant_evaluable")
func intIdentity(_ x: Int) -> Int { x }

func testPartialApply2() -> Int {
  return constArg3(intIdentity)
    // expected-error@-1 {{argument must be a closure}}
}

// Test struct and class constructions. Structs whose initializers are marked as
// constant_evaluable are considered as constants.

struct AStruct {
  var i: Int
}

struct BStruct {
  var i: Int
  @_semantics("constant_evaluable")
  init(_ value: Int) {
    i = value
  }
}

class CClass {
  var str: String
  init() {
    str = ""
  }
}

func testStructAndClasses(arg: Int) {
  constantArgumentFunction(AStruct(i: 9))
    // expected-error@-1 {{argument must be a static method or property of 'AStruct'}}
  constantArgumentFunction(BStruct(340))
  constantArgumentFunction(CClass())
    // expected-error@-1 {{argument must be a static method or property of 'CClass'}}
}

// Test "requires_constant" annotation on protocol requirements.
protocol Proto {
  @_semantics("oslog.requires_constant_arguments")
  func method(arg1: Int, arg2: Bool)
}

struct SConf : Proto {
  func method(arg1: Int, arg2: Bool) { }
}

func testProtocolMethods<T: Proto>(b: Bool, p: T, p2:  Proto, s: SConf) {
  p.method(arg1: 6, arg2: true)
  p.method(arg1: 6, arg2: b)
    // expected-error@-1 {{argument must be a bool literal}}
  p2.method(arg1: 6, arg2: b)
    // expected-error@-1 {{argument must be a bool literal}}
  // Note that even though 's' conforms to Proto, since its method is not
  // annotated as requiring constant arg2, there will be no error here.
  s.method(arg1: 6, arg2: b)
}

// Check requiers annotation on a class method.
class ClassD {
  @_semantics("oslog.requires_constant_arguments")
  func method(_ arg1: Int, _ arg2: Bool)
}

func testClassMethod(d: ClassD, b: Bool) {
  d.method(10, true)
  d.method(10, b)
    // expected-error@-1 {{argument must be a bool literal}}
}

// Test that the check is resilient to errors in the semantics attribute.
@_semantics("oslog.requires_constant_")
func funcWithWrongSemantics(x: Int) {}

func testFunctionWithWrongSemantics(x: Int) {
  funcWithWrongSemantics(x: x)
}

// Test that the check is resilient to other type errors.
func testOtherTypeErrors() {
  constantArgumentFunction(x)
    // expected-error@-1 {{cannot find 'x' in scope}}
  constantArgumentFunction(10 as String)
    // expected-error@-1 {{cannot convert value of type 'Int' to type 'String' in coercion}}
}

// Test constantness of the ordering used in the atomic operations. The atomic
// operations that requires a constant ordering are required to use the
// semantics annotation "atomics.requires_constant_orderings".

internal struct AtomicLoadOrdering {
  @_semantics("constant_evaluable")
  internal static var acquiring: Self { Self() }

  @_semantics("constant_evaluable")
  internal static var sequentiallyConsistent: Self { Self() }
}

internal struct UnsafeAtomicIntStub {
  @_semantics("atomics.requires_constant_orderings")
  internal func load(
    ordering: AtomicLoadOrdering = .sequentiallyConsistent
  ) -> Int {
    return 0
  }
}

func testAtomicOrderingConstantness(
  atomicInt: UnsafeAtomicIntStub,
  myOrder: AtomicLoadOrdering
) {
  _ = atomicInt.load()
  _ = atomicInt.load(ordering: .acquiring)
  _ = atomicInt.load(ordering: .sequentiallyConsistent)
  _ = atomicInt.load(ordering: myOrder)
    // expected-error@-1 {{ordering argument must be a static method or property of 'AtomicLoadOrdering'}}
}
