// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature ThenStatements %s | %FileCheck %s
// RUN: %target-swift-emit-ir -enable-experimental-feature ThenStatements %s

// REQUIRES: swift_feature_ThenStatements

func foo() -> Int {
  if .random() { 1 } else { 2 }
}

// CHECK-LABEL: sil hidden [ossa] @$s7if_expr3fooSiyF : $@convention(thin) () -> Int
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]+]], [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[TRUEBB]]:
// CHECK:       [[ONE_BUILTIN:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
// CHECK:       [[ONE:%[0-9]+]] = apply {{%[0-9]+}}([[ONE_BUILTIN]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[ONE]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       [[TWO_BUILTIN:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 2
// CHECK:       [[TWO:%[0-9]+]] = apply {{%[0-9]+}}([[TWO_BUILTIN]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[TWO]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int

class C {}

func bar(_ x: C) -> C {
  if .random() { x } else { C() }
}

// CHECK-LABEL: sil hidden [ossa] @$s7if_expr3baryAA1CCADF : $@convention(thin) (@guaranteed C) -> @owned C
// CHECK:       bb0([[CPARAM:%[0-9]+]] : @guaranteed $C):
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $C
// CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]+]], [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[TRUEBB]]:
// CHECK:       [[C:%[0-9]+]] = copy_value [[CPARAM]] : $C
// CHECK:       store [[C]] to [init] [[RESULT]] : $*C
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       [[CTOR:%[0-9]+]] = function_ref @$s7if_expr1CCACycfC : $@convention(method) (@thick C.Type) -> @owned C
// CHECK:       [[C:%[0-9]+]] = apply [[CTOR]]({{%[0-9]+}}) : $@convention(method) (@thick C.Type) -> @owned C
// CHECK:       store [[C]] to [init] [[RESULT]] : $*C
// CHECK:       br [[EXITBB]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [take] [[RESULT]] : $*C
// CHECK:       dealloc_stack [[RESULT]] : $*C
// CHECK:       return [[VAL]] : $C

struct Err: Error {}

func baz() throws -> Int {
  if .random() {
    0
  } else if .random() {
    throw Err()
  } else {
    2
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s7if_expr3bazSiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]+]], [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       cond_br {{%[0-9]+}}, [[FALSETRUEBB:bb[0-9]+]], [[FALSEFALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSETRUEBB]]:
// CHECK:       throw {{%[0-9]+}} : $any Error
//
// CHECK:       [[FALSEFALSEBB]]:
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int

func qux() throws -> Int {
  if .random() { 0 } else { try baz() }
}

// CHECK-LABEL: sil hidden [ossa] @$s7if_expr3quxSiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]+]], [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       try_apply {{%[0-9]+}}() : $@convention(thin) () -> (Int, @error any Error), normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
//
// CHECK:       [[NORMALBB]]([[BAZVAL:%[0-9]+]] : $Int):
// CHECK:       store [[BAZVAL]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int
//
// CHECK:       [[ERRORBB]]([[ERR:%[0-9]+]] : @owned $any Error):
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       throw [[ERR]] : $any Error

func optionalVoidCrash() {
  func takesClosure<T>(_ x: () -> T) {}

  struct S {
    func bar() {}
  }

  var s: S?
  takesClosure {
    if true {
      s?.bar()
    } else {
      ()
    }
  }
}

func testClosure() throws -> Int {
  let fn = {
    if .random() {
      0
    } else {
      try baz()
    }
  }
  return try fn()
}

// CHECK-LABEL: sil private [ossa] @$s7if_expr11testClosureSiyKFSiyKcfU_ : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]+]], [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       try_apply {{%[0-9]+}}() : $@convention(thin) () -> (Int, @error any Error), normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
//
// CHECK:       [[NORMALBB]]([[BAZVAL:%[0-9]+]] : $Int):
// CHECK:       store [[BAZVAL]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int
//
// CHECK:       [[ERRORBB]]([[ERR:%[0-9]+]] : @owned $any Error):
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       throw [[ERR]] : $any Error

func testNested() throws -> Int {
  if .random() {
    0
  } else {
    if .random() {
      throw Err()
    } else {
      2
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s7if_expr10testNestedSiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]+]], [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       cond_br {{%[0-9]+}}, [[FALSETRUEBB:bb[0-9]+]], [[FALSEFALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSETRUEBB]]:
// CHECK:       throw {{%[0-9]+}} : $any Error
//
// CHECK:       [[FALSEFALSEBB]]:
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int

func testVar() -> Int {
  let x = if .random() { 1 } else { 2 }
  return x
}

func testPoundIf1() -> Int {
  let x = if .random() {
    #if true
    1
    #else
    ""
    #endif
  } else {
    #if false
    ""
    #else
    2
    #endif
  }
  return x
}

func testPoundIf2() -> Int {
  if .random() {
    #if false
    0
    #else
    #if true
    if .random() { 0 } else { 1 }
    #endif
    #endif
  } else {
    #if true
    if .random() { 0 } else { 1 }
    #endif
  }
}

func testCatch() -> Int {
  do {
    let x = if .random() {
      0
    } else {
      throw Err()
    }
    return x
  } catch {
    return 0
  }
}

struct TestPropertyInit {
  var x = if .random() { 1 } else { 0 }
  lazy var y = if .random() { 1 } else { 0 }
}

func testAssignment() {
  var x = 0
  x = if .random() { 0 } else { 1 }
  let fn = {
    x = if .random() { 0 } else { 1 }
  }
}

func nestedType() throws -> Int {
  if .random() {
    struct S: Error {}
    throw S()
  } else {
    0
  }
}

// MARK: Bindings

enum E {
  case e(Int)
}

struct S {
  var i: Int
  var opt: Int?

  var computed: Int {
    get { i }
    set { i = newValue }
  }
  var coroutined: Int {
    _read { yield i }
    _modify { yield &i }
  }

  subscript(x: Int) -> Int {
    get { i }
    set { i = newValue }
  }

  mutating func testAssign1(_ x: E) {
    i = if case .e(let y) = x { y } else { 0 }
  }


  mutating func testAssign2(_ x: E) {
    i = if case .e(let y) = x { Int(y) } else { 0 }
  }

  func testAssign3(_ x: E) {
    var i = 0
    i = if case .e(let y) = x { y } else { 0 }
    _ = i
  }

  func testAssign4(_ x: E) {
    var i = 0
    let _ = {
      i = if case .e(let y) = x { y } else { 0 }
    }
    _ = i
  }

  mutating func testAssign5(_ x: E) {
    i = switch Bool.random() {
    case true:
      if case .e(let y) = x { y } else { 0 }
    case let z:
      z ? 0 : 1
    }
  }

  mutating func testAssign6(_ x: E) {
    i = if case .e(let y) = x {
      switch Bool.random() {
      case true: y
      case false: y
      }
    } else {
      0
    }
  }

  mutating func testAssign7(_ x: E?) {
    i = if let x = x {
      switch x {
      case .e(let y): y
      }
    } else {
      0
    }
  }

  func testReturn1(_ x: E) -> Int {
    if case .e(let y) = x { y } else { 0 }
  }

  func testReturn2(_ x: E) -> Int {
    return if case .e(let y) = x { y } else { 0 }
  }

  func testReturn3(_ x: E) -> Int {
    {
      if case .e(let y) = x { y } else { 0 }
    }()
  }

  func testReturn4(_ x: E) -> Int {
    return {
      if case .e(let y) = x { y } else { 0 }
    }()
  }

  func testBinding1(_ x: E) -> Int {
    let i = if case .e(let y) = x { y } else { 0 }
    return i
  }

  func testBinding2(_ x: E) -> Int {
    let i = {
      if case .e(let y) = x { y } else { 0 }
    }()
    return i
  }
}

enum G {
  case e(Int)
  case f
}

struct TestLValues {
  var s: S
  var opt: S?
  var optopt: S??

  mutating func testOptPromote1() {
    opt = if .random() { s } else { s }
  }

  mutating func testOptPromote2() {
    optopt = if .random() { s } else { s }
  }

  mutating func testStored1() {
    s.i = if .random() { 1 } else { 0 }
  }

  mutating func testStored2() throws {
    s.i = if .random() { 1 } else { throw Err() }
  }

  mutating func testComputed1() {
    s.computed = if .random() { 1 } else { 0 }
  }

  mutating func testComputed2() throws {
    s.computed = if .random() { 1 } else { throw Err() }
  }

  mutating func testCoroutined1() {
    s.coroutined = if .random() { 1 } else { 0 }
  }

  mutating func testCoroutined2() throws {
    s.coroutined = if .random() { 1 } else { throw Err() }
  }

  mutating func testOptionalChain1() {
    opt?.i = if .random() { 1 } else { 0 }
  }

  mutating func testOptionalChain2() throws {
    opt?.i = if .random() { throw Err() } else { 0 }
  }

  mutating func testOptionalChain3(_ g: G) {
    opt?.i = if case .e(let i) = g { i } else { 0 }
  }

  mutating func testOptionalChain4(_ g: G) throws {
    opt?.i = if case .e(let i) = g { i } else { throw Err() }
  }

  mutating func testOptionalChain5(_ g: G) throws {
    opt?.computed = if case .e(let i) = g { i } else { throw Err() }
  }

  mutating func testOptionalChain6(_ g: G) throws {
    opt?.coroutined = if case .e(let i) = g { i } else { throw Err() }
  }

  mutating func testOptionalChain7() throws {
    optopt??.i = if .random() { 1 } else { throw Err() }
  }

  mutating func testOptionalChain8() throws {
    optopt??.opt = if .random() { 1 } else { throw Err() }
  }

  mutating func testOptionalChain9() throws {
    optopt??.opt? = if .random() { 1 } else { throw Err() }
  }

  mutating func testOptionalForce1() throws {
    opt!.i = if .random() { throw Err() } else { 0 }
  }

  mutating func testOptionalForce2() throws {
    opt!.computed = if .random() { throw Err() } else { 0 }
  }

  mutating func testOptionalForce3(_ g: G) throws {
    opt!.coroutined = if case .e(let i) = g { i } else { throw Err() }
  }

  mutating func testOptionalForce4() throws {
    optopt!!.i = if .random() { 1 } else { throw Err() }
  }

  mutating func testOptionalForce5() throws {
    optopt!!.opt = if .random() { 1 } else { throw Err() }
  }

  mutating func testOptionalForce6() throws {
    optopt!!.opt! = if .random() { 1 } else { throw Err() }
  }

  mutating func testSubscript1() throws {
    s[5] = if .random() { 1 } else { throw Err() }
  }

  mutating func testSubscript2() throws {
    opt?[5] = if .random() { 1 } else { throw Err() }
  }

  mutating func testSubscript3() throws {
    opt![5] = if .random() { 1 } else { throw Err() }
  }

  mutating func testKeyPath1(_ kp: WritableKeyPath<S, Int>) throws {
    s[keyPath: kp] = if .random() { 1 } else { throw Err() }
  }

  mutating func testKeyPath2(_ kp: WritableKeyPath<S, Int>) throws {
    opt?[keyPath: kp] = if .random() { 1 } else { throw Err() }
  }

  mutating func testKeyPath3(_ kp: WritableKeyPath<S, Int>) throws {
    opt![keyPath: kp] = if .random() { 1 } else { throw Err() }
  }
}

func testNever1() -> Never {
  if case let x = fatalError() { x } else { fatalError() }
}

func testNever2() -> Never {
  if .random() { fatalError() } else { fatalError() }
}

func testNever3() -> Int {
  if .random() {
    fatalError()
  } else {
    0
  }
}
// CHECK-LABEL: sil hidden [ossa] @$s7if_expr10testNever3SiyF : $@convention(thin) () -> Int
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       cond_br {{%[0-9]+}}, [[BB_TRUE:bb[0-9]+]], [[BB_FALSE:bb[0-9]+]]
//
// CHECK:       [[BB_TRUE]]:
// CHECK:       function_ref fatalError(_:file:line:)
// CHECK:       unreachable
//
// CHECK:       [[BB_FALSE]]:
// CHECK:       store {{%[0-9]+}} to [trivial] [[RESULT]] : $*Int
// CHECK:       [[RET:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[RET]]

func never() -> Never { fatalError() }

func testNever4() -> Int {
  if .random() {
    never()
  } else {
    0
  }
}

func neverTuple() -> (Never, Int) { fatalError() }

func testNever5() -> (Never, Int) {
  if .random() {
    neverTuple()
  } else {
    (never(), 0)
  }
}
// CHECK-LABEL: sil hidden [ossa] @$s7if_expr10testNever5s5NeverO_SityF : $@convention(thin) () -> (Never, Int)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $(Never, Int)
// CHECK:       cond_br {{%[0-9]+}}, [[BB_TRUE:bb[0-9]+]], [[BB_FALSE:bb[0-9]+]]
//
// CHECK:       [[BB_TRUE]]:
// CHECK:       [[ELT_0:%[0-9]+]] = tuple_element_addr [[RESULT]] : $*(Never, Int), 0
// CHECK:       [[ELT_1:%[0-9]+]] = tuple_element_addr [[RESULT]] : $*(Never, Int), 1
// CHECK:       ([[RET_0:%[0-9]+]], [[RET_1:%[0-9]+]]) = destructure_tuple {{%[0-9]+}} : $(Never, Int)
// CHECK:       store [[RET_0]] to [trivial] [[ELT_0]] : $*Never
// CHECK:       store [[RET_1]] to [trivial] [[ELT_1]] : $*Int
// CHECK:       br [[BB_EXIT:bb[0-9]+]]
//
// CHECK:       [[BB_FALSE]]:
// CHECK:       [[ELT_0:%[0-9]+]] = tuple_element_addr [[RESULT]] : $*(Never, Int), 0
// CHECK:       [[ELT_1:%[0-9]+]] = tuple_element_addr [[RESULT]] : $*(Never, Int), 1
// CHECK:       store {{%[0-9]+}} to [trivial] [[ELT_0]] : $*Never
// CHECK:       store {{%[0-9]+}} to [trivial] [[ELT_1]] : $*Int
// CHECK:       br [[BB_EXIT:bb[0-9]+]]
//
// CHECK:       [[BB_EXIT]]:
// CHECK:       dealloc_stack [[RESULT]] : $*(Never, Int)
// CHECK:       [[RET:%[0-9]+]] = tuple ({{%[0-9]+}} : $Never, {{%[0-9]+}} : $Int)
// CHECK:       return [[RET]]

func testCaptureList() -> Int {
  let fn = { [x = if .random() { 0 } else { 1 }] in x }
  return fn()
}

// https://github.com/apple/swift/issues/68764
func testConditionalCast<T>(_ x: Any) -> T? {
  if .random() {
    x as? T
  } else {
    nil
  }
}

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
}

// rdar://119158202 - Make sure we correctly contextualize local bindings.
func testLazyLocal(_ x: Int?) {
  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr13testLazyLocalyySiSgF1aL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Optional<Int>) -> Int
  lazy var a = if let x { x } else { 0 }
  _ = a

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr13testLazyLocalyySiSgF1bL_SSvg : $@convention(thin) (@guaranteed { var Optional<String> }) -> @owned String
  lazy var b = if .random() {
    let x = ""
    then x
  } else {
    ""
  }
  _ = b

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr13testLazyLocalyySiSgF1cL_SSvg : $@convention(thin) (@guaranteed { var Optional<String> }) -> @owned String
  lazy var c = if .random() {
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr13testLazyLocalyySiSgF1cL_SSvg1xL2_SSvg : $@convention(thin) (@guaranteed { var Optional<String> }) -> @owned String
    lazy var x = ""
    then x
  } else {
    ""
  }
  _ = c

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr13testLazyLocalyySiSgF1dL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Optional<Int>) -> Int
  lazy var d = if .random() {
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr13testLazyLocalyySiSgF1dL_Sivg1yL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Optional<Int>) -> Int
    lazy var y = if let x { x } else { 0 }
    then y
  } else {
    0
  }
  _ = d

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr13testLazyLocalyySiSgF1eL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int
  lazy var e = if .random() {
    @Wrapper
    var x = 0
    then x
  } else {
    0
  }
  _ = e
}

struct LazyProp {
  var a: Int?

  // CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s7if_expr8LazyPropV1bSivg : $@convention(method) (@inout LazyProp) -> Int
  lazy var b = if let a { a } else { 0 }

  // CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s7if_expr8LazyPropV1cSivg : $@convention(method) (@inout LazyProp) -> Int
  lazy var c = if .random() {
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr8LazyPropV1cSivg1xL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int
    lazy var x = 0
    then x
  } else {
    0
  }

  // CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s7if_expr8LazyPropV1dSivg : $@convention(method) (@inout LazyProp) -> Int
  lazy var d = if .random() {
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr8LazyPropV1dSivg1xL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, @inout_aliasable LazyProp) -> Int
    lazy var x = if case let a? = a { a } else { 0 }
    then x
  } else {
    0
  }
}

// https://github.com/swiftlang/swift/issues/75294
func testAsyncLet(_ x: Int?) async {
  async let _ = if let i = x { i } else { 0 }
  // CHECK-LABEL: sil private [ossa] @$s7if_expr12testAsyncLetyySiSgYaFSiyYaYbcfu_ : $@convention(thin) @Sendable @async @substituted <τ_0_0> (Optional<Int>) -> (@out τ_0_0, @error any Error) for <Int>

  async let _ = if let i = x {
    // CHECK-LABEL: sil private [ossa] @$s7if_expr12testAsyncLetyySiSgYaFSiyYaYbcfu0_ : $@convention(thin) @Sendable @async @substituted <τ_0_0> (Optional<Int>) -> (@out τ_0_0, @error any Error) for <Int>
    lazy var y = i
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s7if_expr12testAsyncLetyySiSgYaFSiyYaYbcfu0_1yL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Int) -> Int
    then y
  } else {
    0
  }
}

func testNestedFallthrough1() throws -> Int {
  let x = if .random() {
    switch Bool.random() {
    case true:
      fallthrough
    case false:
      break
    }
    throw Err()
  } else {
    0
  }
  return x
}

func testNestedFallthrough2() throws -> Int {
  let x = if .random() {
    switch Bool.random() {
    case true:
      if .random() {
        fallthrough
      }
      break
    case false:
      break
    }
    throw Err()
  } else {
    0
  }
  return x
}
