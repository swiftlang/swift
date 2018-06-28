// RUN: %target-swift-frontend -emit-sil %s -verify

// ----------------------------------------------------------------------------
// Helper decls.
// ----------------------------------------------------------------------------

// expected-note @+1 {{could not fold operation}}
let globalLet = 1

// expected-note @+2 {{could not fold operation}}
// expected-note @+1 {{could not fold operation}}
var globalVar = 1

@compilerEvaluable
func compilerEvaluableFunc() {}

func nonCompilerEvaluableFunc() {}

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

  init(nonCompilerEvaluableInit: Int) {
    field = nonCompilerEvaluableInit
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
// Actual tests.
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
//   Literals.
// ----------------------------------------------------------------------------

@compilerEvaluable
func test_nilLiteral() {
  let _: Int? = nil
}

@compilerEvaluable
func test_IntegerLiteral() {
  let _ = 1
}

@compilerEvaluable
func test_FloatLiteral() {
  let _ = 1.0
}

@compilerEvaluable
func test_BooleanLiteral() {
  let _ = true
  let _ = false
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_StringLiteral() {
  // expected-note @+1 {{could not fold operation}}
  let _ = "hello world"
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_InterpolatedStringLiteral(x: Int) {
  // expected-note @+1 {{could not fold operation}}
  let _ = "hello world \(x)"
}

@compilerEvaluable
func test_MagicIdentifierLiteral() {
  let _ = #line
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_arrayLiteral() {
  // expected-note @+1 {{could not fold operation}}
  let _ = [1]
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_dictionaryLiteral() {
  // expected-note @+1 {{could not fold operation}}
  let _ = [1: 2]
}

// ----------------------------------------------------------------------------
//   DeclRefs.
// ----------------------------------------------------------------------------

@compilerEvaluable
func test_readArg(arg: Int) {
  let _ = arg
}

@compilerEvaluable
func test_mutateAndReadInout(arg: inout Int) {
  arg = 2
  let _ = arg
}

@compilerEvaluable
func test_readLet() {
  let x = 1
  let _ = x
}

@compilerEvaluable
func test_mutateAndReadVar() {
  var x = 1
  x = 2
  let _ = x
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_readGlobalLet() {
  let _ = globalLet
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_readGlobalVar() {
  let _ = globalVar
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_mutateGlobalVar() {
  globalVar = 3
}

@compilerEvaluable
func test_refCompilerEvaluable() {
  let _ = compilerEvaluableFunc
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_refNonCompilerEvaluable() {
  // expected-note @+1 {{could not fold operation}}
  let _ = nonCompilerEvaluableFunc
}

@compilerEvaluable
func test_refProtocolFunction<T: ProtocolWithAFunction>(t: T) {
  let _ = t.protocolFunction
}

@compilerEvaluable
func test_readOuterArg(arg: Int) {
  func inner() {
    let _ = arg
  }
  inner()
}

@compilerEvaluable
func test_mutateAndReadOuterInout(arg: inout Int) {
  func inner() {
    arg = 2
    let _ = arg
  }
  inner()
}

@compilerEvaluable
func test_readOuterLet() {
  let x = 1
  func inner() {
    let _ = x
  }
  inner()
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_mutateAndReadOuterVar() {
  // expected-note @+2 {{could not fold operation}}
  // expected-note @+1 {{could not fold operation}}
  var x = 1

  // expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
  func inner() {
    x = 2
    let _ = x
  }

  inner()
}

struct DeclRefTestStruct {
  let structLet: Int
  var structVar: Int

  @compilerEvaluable
  func test_readStructLet() {
    let _ = structLet
  }

  @compilerEvaluable
  func test_readStructVar() {
    let _ = structVar
  }

  @compilerEvaluable
  mutating func test_mutateStructVar() {
    structVar = 2
  }
}

func test_refStructSelf() {
  let _ = SimpleStruct.self
}

func test_refStructField(s: SimpleStruct) {
  let _ = s.field
}

// ----------------------------------------------------------------------------
//   Calls.
// ----------------------------------------------------------------------------

@compilerEvaluable
func test_callCompilerEvaluable() {
  compilerEvaluableFunc()
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_callNonCompilerEvaluable() {
  // expected-note @+1 {{could not fold operation}}
  nonCompilerEvaluableFunc()
}

@compilerEvaluable
func test_initStructCompilerEvaluable() {
  let _ = SimpleStruct()
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_initStructNonCompilerEvaluable() {
  // expected-note @+1 {{could not fold operation}}
  let _ = SimpleStruct(nonCompilerEvaluableInit: 1)
}

@compilerEvaluable
func test_initEnum() {
  let _: SimpleEnum = .case1
}

@compilerEvaluable
func test_initEnumWithPayload() {
  let _: PayloadEnum = .case1(x: 2)
}

func test_callMutator() {
  var x = 1
  mutate(&x)
}

@compilerEvaluable
func test_callProtocolFunction<T: ProtocolWithAFunction>(t: T) -> Int {
  return t.protocolFunction()
}

@compilerEvaluable
func test_callTry() throws {
  let _ = try throwingFunction()
}

@compilerEvaluable
func test_callTryForced() {
  let _ = try! throwingFunction()
}

@compilerEvaluable
func test_callTryOptional() {
  let _ = try? throwingFunction()
}

// ----------------------------------------------------------------------------
//   Closures.
// ----------------------------------------------------------------------------

@compilerEvaluable
func test_makeAndCallClosure() {
  let closure = { (i: Int) -> Int in return i + 1 }
  let _ = closure(2)
}

@compilerEvaluable
func test_autoclosure() {
  autocloses(1)
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_escapingClosureCapturesVar() -> () -> () {
  // expected-note @+2 {{could not fold operation}}
  // expected-note @+1 {{could not fold operation}}
  var x = 1

  // expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
  func inner() {
    x = 2
  }
  return inner
}

// ----------------------------------------------------------------------------
//   Misc expressions.
// ----------------------------------------------------------------------------

@compilerEvaluable
func test_constructTuple() {
  let _ = (1, 2)
  let _ = (a: 1, 2)
}

@compilerEvaluable
func test_tupleField() {
  let _ = (1, 2).0
}

@compilerEvaluable
func test_bindOptional(s: SimpleStruct?) {
  let _ = s?.field
}

@compilerEvaluable
func test_forceOptional(x: Int?) {
  let _ = x!
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_keyPath() {
  // expected-note @+1 {{could not fold operation}}
  let _ = \SimpleStruct.field
}

// ----------------------------------------------------------------------------
//   Statements.
// ----------------------------------------------------------------------------

@compilerEvaluable
func test_if(x: Int) -> Int {
  if (x < 0) {
    return -1
  } else {
    return 1
  }
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_forbiddenAfterBranch(x: Int) -> Int {
  if (x < 0) {
    return -1
  } else {
    // expected-note @+1 {{could not fold operation}}
    nonCompilerEvaluableFunc()
    return 1
  }
}

@compilerEvaluable
func test_switchInt(x: Int) -> Int {
  switch (x) {
  case 1: return 1
  case 2: return 2
  default: return 3
  }
}

@compilerEvaluable
func test_switchEnum(x: SimpleEnum) -> Int {
  switch (x) {
  case .case1: return 1
  case .case2: return 2
  }
}

@compilerEvaluable
func test_switchEnumWithPayload(x: PayloadEnum) -> Int {
  switch (x) {
  case .case1(let y): return y
  case .case2(_): return 2
  }
}

@compilerEvaluable
// expected-error @+1 {{@compilerEvaluable function contains forbidden operation}}
func test_whileLoop() -> Int {
  var x = 0
  // expected-note @+1 {{control flow loop found}}
  while (x < 100) {
    x += 1
  }
  return x
}
