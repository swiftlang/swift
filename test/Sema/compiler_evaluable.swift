// RUN: %target-typecheck-verify-swift -module-name ThisModule

// ----------------------------------------------------------------------------
// Test what contexts @compilerEvaluable is allowed in.
// ----------------------------------------------------------------------------

protocol AProtocol {
  @compilerEvaluable
  func protocolFunc() // expected-error{{@compilerEvaluable functions not allowed here}}
}

extension AProtocol {
  @compilerEvaluable
  func extensionFunc() {}
}

class AClass {
  @compilerEvaluable
  init() {} // expected-error{{@compilerEvaluable functions not allowed here}}

  @compilerEvaluable
  func classFunc() {} // expected-error{{@compilerEvaluable functions not allowed here}}
}

extension AClass {
  @compilerEvaluable
  func extensionFunc() {} // expected-error{{@compilerEvaluable functions not allowed here}}
}

struct AStruct {
  @compilerEvaluable
  init() {}

  @compilerEvaluable
  func structFunc() {}

  var structProperty: Int {
    @compilerEvaluable
    get {
      return 42
    }

    @compilerEvaluable
    set(prop) {}
  }

  subscript(i: Int) -> Int {
    @compilerEvaluable
    get {}

    @compilerEvaluable
    set(v) {}
  }
}

extension AStruct {
  @compilerEvaluable
  func extensionFunc() {}
}

struct AGenericStruct<T> {
  @compilerEvaluable
  init() {}

  @compilerEvaluable
  func structFunc() {}
}

extension AGenericStruct {
  @compilerEvaluable
  func extensionFunc() {}
}

enum AEnum {
  case thing1
  case thing2

  @compilerEvaluable
  func enumFunc() {}
}

extension AEnum {
  @compilerEvaluable
  func extensionFunc() {}
}

enum AGenericEnum<T> {
  case thing1
  case thing2

  @compilerEvaluable
  func enumFunc() {}
}

extension AGenericEnum {
  @compilerEvaluable
  func extensionFunc() {}
}

func aFunction() {
  @compilerEvaluable
  func functionFunc() {}
}

func aGenericFunction<T>(t: T) {
  @compilerEvaluable
  func functionFunc() {}
}

@compilerEvaluable
func funcTopLevel() {}

// ----------------------------------------------------------------------------
// Test the AST expression checker.
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
//   Helper decls for expression tests.
// ----------------------------------------------------------------------------

let globalLet = 1
var globalVar = 1
let globalStringLet = "global string"
var globalStringVar = "global string"

@compilerEvaluable
func compilerEvaluable() {}

func nonCompilerEvaluable() {}

@compilerEvaluable
func genericCompilerEvaluable<T>(t: T) {}

func genericNonCompilerEvaluable<T>(t: T) {}

protocol ProtocolWithAFunction {
  func protocolFunction() -> Int
}

@compilerEvaluable
func autocloses(_ x: @autoclosure () -> Int) {}

struct SimpleStruct {
  let field: Int

  @compilerEvaluable
  init() {
    field = 1
  }

  @compilerEvaluable
  static postfix func ... (x: SimpleStruct) -> SimpleStruct {
    return x
  }

  @compilerEvaluable
  func foo() -> Int {
    return 1
  }
}

struct SubscriptableStruct {
  @compilerEvaluable
  init() {}

  @compilerEvaluable
  subscript(x: Int) -> Int { return x }
}

enum SimpleEnum {
  case case1
  case case2
}

enum PayloadEnum {
  case case1(x: Int)
  case case2(x: Int)
}

@compilerEvaluable
func throwingFunction() throws {}

@compilerEvaluable
func mutate(_ x: inout Int) {
  x = 3
}

// ----------------------------------------------------------------------------
//   Actual expression tests.
// ----------------------------------------------------------------------------

@compilerEvaluable
func literals() {
  // NilLiteral
  let _: Int? = nil

  // IntegerLiteral
  let _ = 1

  // FloatLiteral
  let _ = 1.0 // expected-error{{type 'Double' cannot be used in @compilerEvaluable functions}}

  // BooleanLiteral
  let _ = true

  // StringLiteral
  let _ = "hello world" // expected-error{{type 'String' cannot be used in @compilerEvaluable functions}}

  // InterpolatedStringLiteral
  let _ = "hello world \(1)" // expected-error{{type 'String' cannot be used in @compilerEvaluable functions}}

  // MagicIdentifierLiteral
  let _ = #line
}

@compilerEvaluable
func declRef(arg1: Int, arg2: inout Int) {
  let _ = arg1
  let _ = arg2
  arg2 = 1

  let x = 1
  let _ = x

  var y = 1
  let _ = y
  y = 2

  let _ = globalLet
  let _ = globalVar // expected-error{{referencing non-local mutable variables not allowed in @compilerEvaluable functions}}
  globalVar = 2 // expected-error{{referencing non-local mutable variables not allowed in @compilerEvaluable functions}}

  let _ = globalStringLet // expected-error{{type 'String' cannot be used in @compilerEvaluable functions}}
  let _ = globalStringVar // expected-error{{type 'String' cannot be used in @compilerEvaluable functions}}

  let _ = compilerEvaluable
  let _ = nonCompilerEvaluable // expected-error{{@compilerEvaluable functions may not reference non-@compilerEvaluable functions}}

  compilerEvaluable()
  nonCompilerEvaluable() // expected-error{{@compilerEvaluable functions may not reference non-@compilerEvaluable functions}}

  let _: SimpleEnum = .case1
  let _: PayloadEnum = .case1(x: 2)

  func inner() {
    let _ = arg1
    let _ = arg2
    arg2 = 1

    let _ = x
    let _ = y

    let _ = globalLet
    let _ = globalVar // expected-error{{referencing non-local mutable variables not allowed in @compilerEvaluable functions}}
  }

  inner()
}

func declRef2(outerArg1: Int, outerArg2: inout Int) {
  let outerLet = 1
  var outerVar = 1

  @compilerEvaluable
  func inner() {
    let _ = outerArg1
    let _ = outerArg2 // expected-error{{referencing non-local mutable variables not allowed in @compilerEvaluable functions}}
    outerArg2 = 2 // expected-error{{referencing non-local mutable variables not allowed in @compilerEvaluable functions}}

    let _ = outerLet
    let _ = outerVar // expected-error{{referencing non-local mutable variables not allowed in @compilerEvaluable functions}}
    outerVar = 2 // expected-error{{referencing non-local mutable variables not allowed in @compilerEvaluable functions}}
  }
}

struct DeclRefInStruct {
  let field: Int

  @compilerEvaluable
  init() {
    self.init(field: 1)
  }

  @compilerEvaluable
  init(field: Int) {
    self.field = field
  }

  @compilerEvaluable
  init(b: Bool) {
    self.init(field: 1, b: b)  // expected-error{{@compilerEvaluable functions may not reference non-@compilerEvaluable functions}}
  }

  init(field: Int, b: Bool) {
    self.init(field: 1)
  }

  @compilerEvaluable
  func method() -> Int {
    return field
  }
}

enum DeclRefInEnum {
  case thing1
  case thing2

  @compilerEvaluable
  init() {
    self = .thing1
  }

  @compilerEvaluable
  func value() -> Int {
    switch(self) {
    case .thing1:
      return 5
    case .thing2:
      return 10
    }
  }
}

@compilerEvaluable
func declRefToProtocolFunction<T: ProtocolWithAFunction>(t: T) -> Int {
  return t.protocolFunction()
}

@compilerEvaluable
func closures() {
  let x = 2
  let closure = { [x] (i: Int) -> (Int, Int) in
    return (x, i)
  }
  let _ = closure(2)

  autocloses(1)

  var mutable = 3
  func mutate() {
    mutable += 1
  }
  mutate()
}

@compilerEvaluable
func dynamicTypeExpr<T: AProtocol>(t: T) {
  let _ = type(of: t)
}

@compilerEvaluable
func miscAllowedExpressions() throws {
  // DiscardAssignment
  _ = 1

  // DotSyntaxBaseIgnored
  let _ = ThisModule.funcTopLevel

  // MemberRef
  let _ = SimpleStruct().field

  // Try, ForceTry, and OptionalTry
  let _ = try throwingFunction()
  let _ = try! throwingFunction()
  let _ = try? throwingFunction()

  // Paren
  let _ = (1)

  // DotSelf
  let _ = AStruct.self

  // Tuple
  let _ = (1, 2)
  let _ = (a: 1, 2)

  // Subscript
  let _ = SubscriptableStruct()[1]

  // TupleElement
  let _ = (1, 2).0

  // InOut
  var x = 1
  mutate(&x)

  // InjectIntoOptional, BindOptional, OptionalEvaluation, and ForceValue
  let opt: SimpleStruct? = SimpleStruct()
  let _ = opt?.field
  let _ = opt!

  // ConstructorRefCall, Call, PrefixUnary, PostfixUnary, Binary, and
  // DotSyntaxCall
  let simpleStruct = SimpleStruct()
  let _ = compilerEvaluable()
  let _ = !true
  let _ = simpleStruct...
  let _ = 1 + 1
  let _ = simpleStruct.foo()

  // Load
  let _ = x

  // TupleShuffle
  let _: (y: Int, x: Int) = (x: 1, y: 2)

  // Coerce
  let _ = 2 as Int

  // If
  let _ = true ? 1 : 2

  // Assign
  let _ = (x = 2)
}

@compilerEvaluable
func miscForbiddenExpressions() {
  // Array
  let _ = [1] // expected-error{{expression not allowed in @compilerEvaluable functions}}

  // Dictionary
  let _ = ["a": 1] // expected-error{{expression not allowed in @compilerEvaluable functions}}

  // KeyPath and KeyPathApplication
  let keyPath = \SimpleStruct.field // expected-error{{expression not allowed in @compilerEvaluable functions}}
  let _ = SimpleStruct()[keyPath: keyPath] // expected-error{{expression not allowed in @compilerEvaluable functions}}
}

// ----------------------------------------------------------------------------
// Test the AST statement checker.
// ----------------------------------------------------------------------------

@compilerEvaluable
func funcWithLoop() -> Int {
  var x = 1
  while (x < 10) { // expected-error{{loops not allowed in @compilerEvaluable functions}}
    x += 1
  }
  return x
}
