// RUN: %target-typecheck-verify-swift -disable-availability-checking

enum Either<T,U> {
  case first(T)
  case second(U)
}

@_functionBuilder
struct TupleBuilder { // expected-note 2 {{struct 'TupleBuilder' declared here}}
  static func buildBlock() -> () { }
  
  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
  }
  
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildDo<T>(_ value: T) -> T { return value }
  static func buildIf<T>(_ value: T?) -> T? { return value }

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }
}

@_functionBuilder
struct TupleBuilderWithoutIf { // expected-note {{struct 'TupleBuilderWithoutIf' declared here}}
  static func buildBlock() -> () { }
  
  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
  }
  
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildDo<T>(_ value: T) -> T { return value }
}

func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}

func tuplifyWithoutIf<T>(_ cond: Bool, @TupleBuilderWithoutIf body: (Bool) -> T) {
  print(body(cond))
}

func testDiags() {
  // For loop
  tuplify(true) { _ in
    17
    for c in name {
    // expected-error@-1 {{cannot find 'name' in scope}}
    }
  }

  // Declarations
  tuplify(true) { _ in
    17
    let x = 17
    let y: Int // expected-error{{closure containing a declaration cannot be used with function builder 'TupleBuilder'}}
    x + 25
  }

  // Statements unsupported by the particular builder.
  tuplifyWithoutIf(true) {
    if $0 {    // expected-error{{closure containing control flow statement cannot be used with function builder 'TupleBuilderWithoutIf'}}
      "hello"
    }
  }
}

struct A { }
struct B { }

func overloadedTuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) -> A { // expected-note {{found this candidate}}
  return A()
}

func overloadedTuplify<T>(_ cond: Bool, @TupleBuilderWithoutIf body: (Bool) -> T) -> B { // expected-note {{found this candidate}}
  return B()
}

func testOverloading(name: String) {
  let a1 = overloadedTuplify(true) { b in
    if b {
      "Hello, \(name)" 
    }
  }

  let _: A = a1

  _ = overloadedTuplify(true) { b in // expected-error {{ambiguous use of 'overloadedTuplify(_:body:)'}}
    b ? "Hello, \(name)" : "Goodbye"
    42
    overloadedTuplify(false) {
      $0 ? "Hello, \(name)" : "Goodbye"
      42
      if $0 {
        "Hello, \(name)" 
      }
    }
  }
}

protocol P {
  associatedtype T
}

struct AnyP : P {
  typealias T = Any
  init<T>(_: T) where T : P {}
}

struct TupleP<U> : P {
  typealias T = U
  init(_: U) {}
}

@_functionBuilder
struct Builder {
  static func buildBlock<S0, S1>(_ stmt1: S0, _ stmt2: S1) // expected-note {{required by static method 'buildBlock' where 'S1' = 'Label<_>.Type'}}
           -> TupleP<(S0, S1)> where S0: P, S1: P {
    return TupleP((stmt1, stmt2))
  }
}

struct G<C> : P where C : P {
  typealias T = C
  init(@Builder _: () -> C) {}
}

struct Text : P {
  typealias T = String
  init(_: T) {}
}

struct Label<L> : P where L : P { // expected-note 2 {{'L' declared as parameter to type 'Label'}}
  typealias T = L
  init(@Builder _: () -> L) {} // expected-note {{'init(_:)' declared here}}
}

func test_51167632() -> some P {
  AnyP(G { // expected-error {{type 'Label<_>.Type' cannot conform to 'P'; only struct/enum/class types can conform to protocols}}
    Text("hello")
    Label  // expected-error {{generic parameter 'L' could not be inferred}}
    // expected-note@-1 {{explicitly specify the generic arguments to fix this issue}} {{10-10=<<#L: P#>>}}
  })
}

func test_56221372() -> some P {
  AnyP(G {
    Text("hello")
    Label() // expected-error {{generic parameter 'L' could not be inferred}}
    // expected-error@-1 {{missing argument for parameter #1 in call}}
    // expected-note@-2 {{explicitly specify the generic arguments to fix this issue}} {{10-10=<<#L: P#>>}}
  })
}

struct SR11440 {
  typealias ReturnsTuple<T> = () -> (T, T)
  subscript<T, U>(@TupleBuilder x: ReturnsTuple<T>) -> (ReturnsTuple<U>) -> Void { //expected-note {{in call to 'subscript(_:)'}}
    return { _ in }
  }

  func foo() {
    // This is okay, we apply the function builder for the subscript arg.
    self[{
      5
      5
    }]({
      (5, 5)
    })

    // But we shouldn't perform the transform for the argument to the call
    // made on the function returned from the subscript.
    self[{ // expected-error {{generic parameter 'U' could not be inferred}}
      5
      5
    }]({
      5
      5
    })
  }
}

func acceptInt(_: Int, _: () -> Void) { }

// SR-11350 crash due to improper recontextualization.
func erroneousSR11350(x: Int) {
  tuplify(true) { b in
    17
    x + 25
    Optional(tuplify(false) { b in
      if b {
        acceptInt(0) { }
      }
    }).domap(0) // expected-error{{value of type '()?' has no member 'domap'}}
  }
}

func extraArg() {
  tuplify(true) { _ in
    1
    2
    3
    4
    5
    6 // expected-error {{extra argument in call}}
  }
}

// rdar://problem/53209000 - use of #warning and #error
tuplify(true) { x in
  1
  #error("boom")    // expected-error{{boom}}
  "hello"
  #warning("oops")  // expected-warning{{oops}}
  3.14159
}

struct MyTuplifiedStruct {
  var condition: Bool

  @TupleBuilder var computed: some Any { // expected-note{{remove the attribute to explicitly disable the function builder}}{{3-17=}}
    if condition {
      return 17 // expected-warning{{application of function builder 'TupleBuilder' disabled by explicit 'return' statement}}
      // expected-note@-1{{remove 'return' statements to apply the function builder}}{{7-14=}}{{12-19=}}
    } else {
           return 42
    }
  }
}

// Check that we're performing syntactic use diagnostics.
func acceptMetatype<T>(_: T.Type) -> Bool { true }

func syntacticUses<T>(_: T) {
  tuplify(true) { x in
    if x && acceptMetatype(T) { // expected-error{{expected member name or constructor call after type name}}
      // expected-note@-1{{use '.self' to reference the type object}}
      acceptMetatype(T) // expected-error{{expected member name or constructor call after type name}}
      // expected-note@-1{{use '.self' to reference the type object}}
    }
  }
}

// Check custom diagnostics within "if" conditions.
struct HasProperty {
  var property: Bool = false
}

func checkConditions(cond: Bool) {
  var x = HasProperty()

  tuplify(cond) { value in
    if x.property = value { // expected-error{{use of '=' in a boolean context, did you mean '=='?}}
      "matched it"
    }
  }
}

// Check that a closure with a single "return" works with function builders.
func checkSingleReturn(cond: Bool) {
  tuplify(cond) { value in
    return (value, 17)
  }

  tuplify(cond) { value in
    (value, 17)
  }

  tuplify(cond) {
    ($0, 17)
  }
}

// rdar://problem/59116520
func checkImplicitSelfInClosure() {
  @_functionBuilder
  struct Builder {
    static func buildBlock(_ children: String...) -> Element { Element() }
  }

  struct Element {
    static func nonEscapingClosure(@Builder closure: (() -> Element)) {}
    static func escapingClosure(@Builder closure: @escaping (() -> Element)) {}
  }

  class C {
    let identifier: String = ""

    func testImplicitSelf() {
      Element.nonEscapingClosure {
        identifier // okay
      }

      Element.escapingClosure { // expected-note {{capture 'self' explicitly to enable implicit 'self' in this closure}}
        identifier // expected-error {{reference to property 'identifier' in closure requires explicit use of 'self' to make capture semantics explicit}}
        // expected-note@-1 {{reference 'self.' explicitly}}
      }
    }
  }
}

// rdar://problem/59239224 - crash because some nodes don't have type
// information during solution application.
struct X<T> {
  init(_: T) { }
}

@TupleBuilder func foo(cond: Bool) -> some Any {
  if cond {
    tuplify(cond) { x in
      X(x)
    }
  }
}

// switch statements don't allow fallthrough
enum E {
  case a
  case b(Int, String?)
}

func testSwitch(e: E) {
  tuplify(true) { c in
    "testSwitch"
    switch e {
    case .a:
      "a"
    case .b(let i, let s?):
      i * 2
      s + "!"
      fallthrough // expected-error{{closure containing control flow statement cannot be used with function builder 'TupleBuilder'}}
    case .b(let i, nil):
      "just \(i)"
    }
  }
}

// Ensure that we don't back-propagate constraints to the subject
// expression. This is a potential avenue for future exploration, but
// is currently not supported by switch statements outside of function
// builders. It's better to be consistent for now.
enum E2 {
  case b(Int, String?) // expected-note{{'b' declared here}}
}

func getSomeEnumOverloaded(_: Double) -> E { return .a }
func getSomeEnumOverloaded(_: Int) -> E2 { return .b(0, nil) }

func testOverloadedSwitch() {
  tuplify(true) { c in
    // FIXME: Bad source location.
    switch getSomeEnumOverloaded(17) { // expected-error{{type 'E2' has no member 'a'; did you mean 'b'?}}
    case .a:
      "a"
    default:
      "default"
    }
  }
}

// Check exhaustivity.
func testNonExhaustiveSwitch(e: E) {
    tuplify(true) { c in
    "testSwitch"
    switch e { // expected-error{{switch must be exhaustive}}
      // expected-note @-1{{add missing case: '.b(_, .none)'}}
    case .a:
      "a"
    case .b(let i, let s?):
      i * 2
      s + "!"
    }
  }
}

// rdar://problem/59856491
struct TestConstraintGenerationErrors {
  @TupleBuilder var buildTupleFnBody: String {
    let a = nil // There is no diagnostic here because next line fails to pre-check, so body is invalid
    String(nothing) // expected-error {{cannot find 'nothing' in scope}}
  }

  @TupleBuilder var nilWithoutContext: String {
    let a = nil // expected-error {{'nil' requires a contextual type}}
  }

  func buildTupleClosure() {
    tuplify(true) { _ in
      let a = nothing // expected-error {{cannot find 'nothing' in scope}}
      String(nothing) // expected-error {{cannot find 'nothing' in scope}}
    }
  }
}

// Check @unknown
func testUnknownInSwitchSwitch(e: E) {
    tuplify(true) { c in
    "testSwitch"
    switch e {
    @unknown case .a: // expected-error{{'@unknown' is only supported for catch-all cases ("case _")}}
      "a"
    case .b(let i, let s?):
      i * 2
      s + "!"
    default:
      "nothing"
    }
  }
}

// Check for mutability mismatches when there are multiple case items
// referring to same-named variables.
enum E3 {
  case a(Int, String)
  case b(String, Int)
  case c(String, Int)
}

func testCaseMutabilityMismatches(e: E3) {
    tuplify(true) { c in
    "testSwitch"
    switch e {
    case .a(let x, var y),
         .b(let y, // expected-error{{'let' pattern binding must match previous 'var' pattern binding}}
            var x), // expected-error{{'var' pattern binding must match previous 'let' pattern binding}}
         .c(let y, // expected-error{{'let' pattern binding must match previous 'var' pattern binding}}
            var x): // expected-error{{'var' pattern binding must match previous 'let' pattern binding}}
      x
      y += "a"
    }
  }
}

// Check for type equivalence among different case variables with the same name.
func testCaseVarTypes(e: E3) {
    // FIXME: Terrible diagnostic
    tuplify(true) { c in  // expected-error{{type of expression is ambiguous without more context}}
    "testSwitch"
    switch e {
    case .a(let x, let y),
         .c(let x, let y):
      x
      y + "a"
    }
  }
}

// Test for buildFinalResult.
@_functionBuilder
struct WrapperBuilder {
  static func buildBlock() -> () { }
  
  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
  }
  
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildDo<T>(_ value: T) -> T { return value }
  static func buildIf<T>(_ value: T?) -> T? { return value }

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }
  static func buildFinalResult<T>(_ value: T) -> Wrapper<T> {
    return Wrapper(value: value)
  }
}

struct Wrapper<T> {
  var value: T
}

func wrapperify<T>(_ cond: Bool, @WrapperBuilder body: (Bool) -> T) -> T{
  return body(cond)
}

func testWrapperBuilder() {
  let x = wrapperify(true) { c in
    3.14159
    "hello"
  }

  let _: Int = x // expected-error{{cannot convert value of type 'Wrapper<(Double, String)>' to specified type 'Int'}}
}

// rdar://problem/61347993 - empty function builder doesn't compile
func rdar61347993() {
  struct Result {}

  @_functionBuilder
  struct Builder {
    static func buildBlock() -> Result {
      Result()
    }
  }

  func test_builder<T>(@Builder _: () -> T) {}
  test_builder {} // Ok

  func test_closure(_: () -> Result) {}
  test_closure {} // expected-error {{cannot convert value of type '()' to closure result type 'Result'}}
}

// One-way constraints through parameters.
func wrapperifyInfer<T, U>(_ cond: Bool, @WrapperBuilder body: (U) -> T) -> T {
  fatalError("boom")
}

let intValue = 17
wrapperifyInfer(true) { x in // expected-error{{unable to infer type of a closure parameter 'x' in the current context}}
  intValue + x
}

struct DoesNotConform {}

struct MyView {
  @TupleBuilder var value: some P { // expected-error {{return type of property 'value' requires that 'DoesNotConform' conform to 'P'}}
    // expected-note@-1 {{opaque return type declared here}}
    DoesNotConform()
  }

  @TupleBuilder func test() -> some P { // expected-error {{return type of instance method 'test()' requires that 'DoesNotConform' conform to 'P'}}
    // expected-note@-1 {{opaque return type declared here}}
    DoesNotConform()
  }

  @TupleBuilder var emptySwitch: some P {
    switch Optional.some(1) { // expected-error {{'switch' statement body must have at least one 'case' or 'default' block; do you want to add a default case?}}
    }
  }

  @TupleBuilder var invalidSwitchOne: some P {
    switch Optional.some(1) {
    case . // expected-error {{expected ':' after 'case'}}
    } // expected-error {{expected identifier after '.' expression}}
  }

  @TupleBuilder var invalidSwitchMultiple: some P {
    switch Optional.some(1) {
    case .none: // expected-error {{'case' label in a 'switch' should have at least one executable statement}}
    case . // expected-error {{expected ':' after 'case'}}
    } // expected-error {{expected identifier after '.' expression}}
  }

  @TupleBuilder var invalidCaseWithoutDot: some P {
    switch Optional.some(1) {
    case none: 42 // expected-error {{cannot find 'none' in scope}}
    case .some(let x):
      0
    }
  }

  @TupleBuilder var invalidConversion: Int {
    "" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
  }
}
