// RUN: %target-typecheck-verify-swift -module-name ThisModule

// ----------------------------------------------------------------------------
// Test what functions @compilerEvaluable is allowed on.
// ----------------------------------------------------------------------------

protocol AProtocol {
  @compilerEvaluable
  func protocolFunc() // expected-error{{protocol requirements cannot be declared @compilerEvaluable}}
}

extension AProtocol {
  @compilerEvaluable
  func extensionFunc() {}
}

class AClass {
  @compilerEvaluable
  init() {} // expected-error{{@compilerEvaluable function not allowed in 'AClass'}}
  // expected-note @-1 {{type 'AClass' cannot be used because it is a class}}

  @compilerEvaluable
  func classFunc() {} // expected-error{{@compilerEvaluable function not allowed in 'AClass'}}
  // expected-note @-1 {{type 'AClass' cannot be used because it is a class}}
}

extension AClass {
  @compilerEvaluable
  func extensionFunc() {} // expected-error{{@compilerEvaluable function not allowed in 'AClass'}}
  // expected-note @-1 {{type 'AClass' cannot be used because it is a class}}
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

struct StructWithBadSubscriptParam {
  @compilerEvaluable
  subscript(i: AClass) -> Int { return 1 } // expected-error{{@compilerEvaluable function not allowed to have parameter of type 'AClass'}}
  // expected-note @-1 {{type 'AClass' cannot be used because it is a class}}
}

struct StructWithBadSubsriptResult {
  @compilerEvaluable
  subscript(i: Int) -> AClass { return AClass() } // expected-error{{@compilerEvaluable function not allowed to have result of type 'AClass'}}
  // expected-note @-1 {{type 'AClass' cannot be used because it is a class}}
}

@compilerEvaluable
func funcWithBadParam(i: AClass) -> Int { // expected-error{{@compilerEvaluable function not allowed to have parameter of type 'AClass'}}
// expected-note @-1 {{type 'AClass' cannot be used because it is a class}}
  return 1
}

@compilerEvaluable
func funcWithBadResult() -> AClass { // expected-error{{@compilerEvaluable function not allowed to have result of type 'AClass'}}
// expected-note @-1 {{type 'AClass' cannot be used because it is a class}}
  return AClass()
}

// ----------------------------------------------------------------------------
// Test the compiler-representable type checker.
// ----------------------------------------------------------------------------

class NotRepresentableClass {}

enum NotRepresentableEnum {
  case case1
  case case2(field: NotRepresentableClass)
}

struct NotRepresentableStruct {
  let field: NotRepresentableClass
}

enum RepresentableEnum {
  case case1
  case case2(field: Int)
}

struct RepresentableStruct {
  let field: Int

  @compilerEvaluable
  init(field: Int) {
    self.field = field
  }
}

enum GenericRepresentableEnum<T> {
  case case1
  case case2(field: Int, genericField: T)
}

struct GenericRepresentableStruct<T> {
  let field: Int
  let genericField: T

  @compilerEvaluable
  init(field: Int, genericField: T) {
    self.field = field
    self.genericField = genericField
  }
}

@compilerEvaluable
func testCompilerRepresentableChecker() {
  let _ = NotRepresentableClass() // expected-error {{type 'NotRepresentableClass' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _: NotRepresentableEnum = .case1 // expected-error {{type 'NotRepresentableEnum' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{enum 'NotRepresentableEnum' cannot be used because it has element 'case2' with payload 'NotRepresentableClass'}}
  // expected-note @-2 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _ = NotRepresentableStruct(field: NotRepresentableClass()) // expected-error {{type 'NotRepresentableStruct' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{struct 'NotRepresentableStruct' cannot be used because it has field 'field' with type 'NotRepresentableClass'}}
  // expected-note @-2 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _: RepresentableEnum = .case1

  let _ = RepresentableStruct(field: 3)

  let _: GenericRepresentableEnum<Int> = .case1

  let _ = GenericRepresentableStruct(field: 1, genericField: 2)

  let _: GenericRepresentableEnum<NotRepresentableClass> = .case1 // expected-error {{type 'GenericRepresentableEnum<NotRepresentableClass>' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'GenericRepresentableEnum<NotRepresentableClass>' cannot be used because it has generic argument 'NotRepresentableClass'}}
  // expected-note @-2 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _ = NotRepresentableClass.self // expected-error {{type 'NotRepresentableClass.Type' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{metatype 'NotRepresentableClass.Type' cannot be used because it is the metatype of 'NotRepresentableClass'}}
  // expected-note @-2 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _ = RepresentableStruct.self

  let _ = GenericRepresentableStruct<Int>.self

  let _ = GenericRepresentableStruct<NotRepresentableClass>.self // expected-error {{type 'GenericRepresentableStruct<NotRepresentableClass>.Type' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{metatype 'GenericRepresentableStruct<NotRepresentableClass>.Type' cannot be used because it is the metatype of 'GenericRepresentableStruct<NotRepresentableClass>'}}
  // expected-note @-2 {{type 'GenericRepresentableStruct<NotRepresentableClass>' cannot be used because it has generic argument 'NotRepresentableClass'}}
  // expected-note @-3 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _ = (RepresentableStruct(field: 3), 2)

  let _ = (NotRepresentableClass(), 2) // expected-error {{type '(NotRepresentableClass, Int)' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{tuple '(NotRepresentableClass, Int)' cannot be used because it has element 'NotRepresentableClass'}}
  // expected-note @-2 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _ = { (x: RepresentableStruct) -> RepresentableStruct in return x }

  let _ = { (x: NotRepresentableClass) -> Int in return 1 } // expected-error {{type '(NotRepresentableClass) -> Int' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{function type '(NotRepresentableClass) -> Int' cannot be used because it has parameter type 'NotRepresentableClass'}}
  // expected-note @-2 {{type 'NotRepresentableClass' cannot be used because it is a class}}

  let _ = { (x: Int) -> NotRepresentableClass in return NotRepresentableClass() } // expected-error {{type '(Int) -> NotRepresentableClass' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{function type '(Int) -> NotRepresentableClass' cannot be used because it has result type 'NotRepresentableClass'}}
  // expected-note @-2 {{type 'NotRepresentableClass' cannot be used because it is a class}}
}

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
  // expected-note @-1 {{type 'Double' is an unsupported standard library type}}

  // BooleanLiteral
  let _ = true

  // StringLiteral
  let _ = "hello world" // expected-error{{type 'String' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'String' is an unsupported standard library type}}

  // InterpolatedStringLiteral
  let _ = "hello world \(1)" // expected-error{{type 'String' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'String' is an unsupported standard library type}}

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
  // expected-note @-1 {{type 'String' is an unsupported standard library type}}
  let _ = globalStringVar // expected-error{{type 'String' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'String' is an unsupported standard library type}}

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
  let _ = [1] // expected-error{{type '[Int]' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'Array' is an unsupported standard library type}}

  // Dictionary
  let _ = ["a": 1] // expected-error{{type '[String : Int]' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'Dictionary' is an unsupported standard library type}}

  // KeyPath and KeyPathApplication
  let keyPath = \SimpleStruct.field // expected-error{{type 'KeyPath<SimpleStruct, Int>' cannot be used in @compilerEvaluable functions}}
  // expected-note @-1 {{type 'KeyPath<SimpleStruct, Int>' cannot be used because it is a class}}
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
