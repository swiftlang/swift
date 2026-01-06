// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature ThenStatements %s | %FileCheck %s
// RUN: %target-swift-emit-ir -enable-experimental-feature ThenStatements %s

// REQUIRES: swift_feature_ThenStatements

func foo() -> Int {
  switch Bool.random() {
    case true:
      1
    case false:
      2
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr3fooSiyF : $@convention(thin) () -> Int
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_value {{%[0-9]+}} : $Builtin.Int1, case {{%[0-9]+}}: [[TRUEBB:bb[0-9]+]], case {{%[0-9]+}}: [[FALSEBB:bb[0-9]+]]
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
  switch Bool.random() {
    case true:
      x
    case false:
      C()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr3baryAA1CCADF : $@convention(thin) (@guaranteed C) -> @owned C
// CHECK:       bb0([[CPARAM:%[0-9]+]] : @guaranteed $C):
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $C
// CHECK:       switch_value {{%[0-9]+}} : $Builtin.Int1, case {{%[0-9]+}}: [[TRUEBB:bb[0-9]+]], case {{%[0-9]+}}: [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[TRUEBB]]:
// CHECK:       [[C:%[0-9]+]] = copy_value [[CPARAM]] : $C
// CHECK:       store [[C]] to [init] [[RESULT]] : $*C
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       [[CTOR:%[0-9]+]] = function_ref @$s11switch_expr1CCACycfC : $@convention(method) (@thick C.Type) -> @owned C
// CHECK:       [[C:%[0-9]+]] = apply [[CTOR]]({{%[0-9]+}}) : $@convention(method) (@thick C.Type) -> @owned C
// CHECK:       store [[C]] to [init] [[RESULT]] : $*C
// CHECK:       br [[EXITBB]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [take] [[RESULT]] : $*C
// CHECK:       dealloc_stack [[RESULT]] : $*C
// CHECK:       return [[VAL]] : $C

struct Err: Error {}

enum E { case a, b, c }

func baz(_ e: E) throws -> Int {
  switch e {
  case .a:
    1
  case .b:
    throw Err()
  default:
    2
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr3bazySiAA1EOKF : $@convention(thin) (E) -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_enum %0 : $E, case #E.a!enumelt: [[ABB:bb[0-9]+]], case #E.b!enumelt: [[BBB:bb[0-9]+]], default [[DEFBB:bb[0-9]+]]
//
// CHECK:       [[ABB]]:
// CHECK:       [[ONE_BUILTIN:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
// CHECK:       [[ONE:%[0-9]+]] = apply {{%[0-9]+}}([[ONE_BUILTIN]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[ONE]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[BBB]]:
// CHECK:       throw {{%[0-9]+}} : $any Error
//
// CHECK:       [[DEFBB]]:
// CHECK:       [[TWO_BUILTIN:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 2
// CHECK:       [[TWO:%[0-9]+]] = apply {{%[0-9]+}}([[TWO_BUILTIN]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[TWO]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int

func qux() throws -> Int {
  switch Bool.random() {
    case true:
      0
    case false:
      try baz(.a)
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr3quxSiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_value {{%[0-9]+}} : $Builtin.Int1, case {{%[0-9]+}}: [[TRUEBB:bb[0-9]+]], case {{%[0-9]+}}: [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       try_apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(thin) (E) -> (Int, @error any Error), normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
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

func testFallthrough() throws -> Int {
  switch Bool.random() {
  case true:
    if .random() { fallthrough }
    throw Err()
  case false:
    1
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr15testFallthroughSiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_value {{%[0-9]+}} : $Builtin.Int1, case {{%[0-9]+}}: [[TRUEBB:bb[0-9]+]], case {{%[0-9]+}}: [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[TRUEBB]]:
// CHECK:       cond_br {{.*}}, [[IFTRUEBB:bb[0-9]+]], [[IFFALSEBB:bb[0-9]+]]
//
// CHECK:       [[IFTRUEBB]]:
// CHECK:       br [[ACTUALFALSEBB:bb[0-9]+]]
//
// CHECK:       [[IFFALSEBB]]:
// CHECK:       throw {{%[0-9]+}} : $any Error
//
// CHECK:       [[FALSEBB]]:
// CHECK:       br [[ACTUALFALSEBB]]
//
// CHECK:       [[ACTUALFALSEBB]]:
// CHECK:       [[ONE_BUILTIN:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
// CHECK:       [[ONE:%[0-9]+]] = apply {{%[0-9]+}}([[ONE_BUILTIN]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[ONE]] to [trivial] [[RESULT]] : $*Int
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int

func testFallthrough2() -> Int {
  let x = switch Bool.random() {
  case true:
    fallthrough
  case false:
    1
  }
  return x
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr16testFallthrough2SiyF : $@convention(thin) () -> Int {
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_value {{%[0-9]+}} : $Builtin.Int1, case {{%[0-9]+}}: [[TRUEBB:bb[0-9]+]], case {{%[0-9]+}}: [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[TRUEBB]]:
// CHECK-NEXT:  br [[ENDBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK-NEXT:  br [[ENDBB]]
//
// CHECK:       [[ENDBB]]:
// CHECK:       [[ONELIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
// CHECK:       [[ONE:%[0-9]+]] = apply {{%[0-9]+}}([[ONELIT]], {{%[0-9]+}})
// CHECK:       store [[ONE]] to [trivial] [[RESULT]] : $*Int
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       [[VAL_RESULT:%[0-9]+]] = move_value [var_decl] [[VAL]] : $Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL_RESULT:[%0-9]+]] : $Int

func testFallthrough3() throws -> Int {
  switch Bool.random() {
  case true:
    switch Bool.random() {
    case true:
      if .random() {
        fallthrough
      }
      throw Err()
    case false:
      1
    }
  case false:
    0
  }
}

func testClosure() throws -> Int {
  let fn = {
    switch Bool.random() {
    case true:
      0
    case false:
      try baz(.a)
    }
  }
  return try fn()
}

// CHECK-LABEL: sil private [ossa] @$s11switch_expr11testClosureSiyKFSiyKcfU_ : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_value {{%[0-9]+}} : $Builtin.Int1, case {{%[0-9]+}}: [[TRUEBB:bb[0-9]+]], case {{%[0-9]+}}: [[FALSEBB:bb[0-9]+]]
//
// CHECK:       [[FALSEBB]]:
// CHECK:       try_apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(thin) (E) -> (Int, @error any Error), normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
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

func testVar1() -> Int {
  let x = switch Bool.random() {
    case let b where b == true:
      1
    case false:
      0
    default:
      2
  }
  return x
}

func testVar2() -> Int {
  let x = switch Bool.random() {
    case let b where b == true:
      1
    case false:
      0
    default:
      2
  }
  return x
}

func testCatch() -> Int {
  do {
    let x = switch Bool.random() {
    case true:
      0
    case false:
      throw Err()
    }
    return x
  } catch {
    return 0
  }
}

struct TestPropertyInit {
  var x = switch Bool.random() {
    case let b where b == true:
      1
    case false:
      0
    default:
      2
  }
  lazy var y = switch Bool.random() {
    case let b where b == true:
      1
    case false:
      0
    default:
      2
  }
}

// https://github.com/swiftlang/swift/issues/75294
func testAsyncLet(_ x: Int?) async {
  async let _ = switch x { case let i?: i default: 0 }
  // CHECK-LABEL: sil private [ossa] @$s11switch_expr12testAsyncLetyySiSgYaFSiyYaYbcfu_ : $@convention(thin) @Sendable @async @substituted <τ_0_0> (Optional<Int>) -> (@out τ_0_0, @error any Error) for <Int>
}

func testNested(_ e: E) throws -> Int {
  switch e {
  case .a:
    1
  default:
    switch e {
    case .b:
      throw Err()
    default:
      2
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr10testNestedySiAA1EOKF : $@convention(thin) (E) -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_enum %0 : $E, case #E.a!enumelt: [[ABB:bb[0-9]+]], default [[DEFBB:bb[0-9]+]]
//
// CHECK:       [[ABB]]:
// CHECK:       [[ONE_BUILTIN:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
// CHECK:       [[ONE:%[0-9]+]] = apply {{%[0-9]+}}([[ONE_BUILTIN]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[ONE]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[DEFBB]]({{.*}}):
// CHECK:       [[NESTEDRESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_enum %0 : $E, case #E.b!enumelt: [[BBB:bb[0-9]+]], default [[NESTEDDEFBB:bb[0-9]+]]

// CHECK:       [[BBB]]:
// CHECK:       throw {{%[0-9]+}} : $any Error
//
// CHECK:       [[NESTEDDEFBB]]({{.*}}):
// CHECK:       [[TWO_BUILTIN:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 2
// CHECK:       [[TWO:%[0-9]+]] = apply {{%[0-9]+}}([[TWO_BUILTIN]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[TWO]] to [trivial] [[NESTEDRESULT]] : $*Int
// CHECK:       [[TMP:%[0-9]+]] = load [trivial] [[NESTEDRESULT]] : $*Int
// CHECK:       store [[TMP]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[EXITBB:bb[0-9]+]]
//
// CHECK:       [[EXITBB]]:
// CHECK:       [[VAL:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[VAL]] : $Int

func testPoundIf1() -> Int {
  let x = switch Bool.random() {
  case true:
    #if true
    1
    #else
    ""
    #endif
  case false:
    #if false
    ""
    #else
    2
    #endif
  }
  return x
}

func testPoundIf2() -> Int {
  switch Bool.random() {
  case true:
    #if false
    0
    #else
    #if true
    switch Bool.random() { case true: 0 case false: 1 }
    #endif
    #endif
  case false:
    #if true
    switch Bool.random() { case true: 0 case false: 1 }
    #endif
  }
}

func testAssignment() {
  var x = 0
  x = switch Bool.random() { case true: 0 case false: 1 }
  let fn = {
    x = switch Bool.random() { case true: 0 case false: 1 }
  }
}

func nestedType() throws -> Int {
  switch Bool.random() {
  case true:
    struct S: Error {}
    throw S()
  case false:
    0
  }
}

// MARK: Bindings

enum F {
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

  mutating func testAssign1(_ x: F) {
    i = switch x {
    case .e(let y): y
    }
  }

  mutating func testAssign2(_ x: F) {
    i = switch x {
    case .e(let y): Int(y)
    }
  }

  func testAssign3(_ x: F) {
    var i = 0
    i = switch x {
    case .e(let y): y
    }
    _ = i
  }

  func testAssign4(_ x: F) {
    var i = 0
    let _ = {
      i = switch x {
      case .e(let y): y
      }
    }
    _ = i
  }

  mutating func testAssign5(_ x: F) {
    i = switch Bool.random() {
    case true:
      switch x {
      case .e(let y): y
      }
    case let z:
      z ? 0 : 1
    }
  }

  mutating func testAssign6(_ x: F) {
    i = switch x {
    case .e(let y):
      switch Bool.random() {
      case true: y
      case false: y
      }
    }
  }

  func testReturn1(_ x: F) -> Int {
    switch x {
    case .e(let y): y
    }
  }

  func testReturn2(_ x: F) -> Int {
    return switch x {
    case .e(let y): y
    }
  }

  func testReturn3(_ x: F) -> Int {
    {
      switch x {
      case .e(let y): y
      }
    }()
  }

  func testReturn4(_ x: F) -> Int {
    return {
      switch x {
      case .e(let y): y
      }
    }()
  }

  func testBinding1(_ x: F) -> Int {
    let i = switch x {
    case .e(let y): y
    }
    return i
  }

  func testBinding2(_ x: F) -> Int {
    let i = {
      switch x {
      case .e(let y): y
      }
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
    opt = switch Bool.random() { case true: s case false: s }
  }

  mutating func testOptPromote2() {
    optopt = switch Bool.random() { case true: s case false: s }
  }

  mutating func testStored1() {
    s.i = switch Bool.random() { case true: 1 case false: 0 }
  }

  mutating func testStored2() throws {
    s.i = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testComputed1() {
    s.computed = switch Bool.random() { case true: 1 case false: 0 }
  }

  mutating func testComputed2() throws {
    s.computed = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testCoroutined1() {
    s.coroutined = switch Bool.random() { case true: 1 case false: 0 }
  }

  mutating func testCoroutined2() throws {
    s.coroutined = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testOptionalChain1() {
    opt?.i = switch Bool.random() { case true: 1 case false: 0 }
  }

  mutating func testOptionalChain2() throws {
    opt?.i = switch Bool.random() { case true: throw Err() case false: 0 }
  }

  mutating func testOptionalChain3(_ g: G) {
    opt?.i = switch g { case .e(let i): i default: 0 }
  }

  mutating func testOptionalChain4(_ g: G) throws {
    opt?.i = switch g { case .e(let i): i default: throw Err() }
  }

  mutating func testOptionalChain5(_ g: G) throws {
    opt?.computed = switch g { case .e(let i): i default: throw Err() }
  }

  mutating func testOptionalChain6(_ g: G) throws {
    opt?.coroutined = switch g { case .e(let i): i default: throw Err() }
  }

  mutating func testOptionalChain7() throws {
    optopt??.i = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testOptionalChain8() throws {
    optopt??.opt = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testOptionalChain9() throws {
    optopt??.opt? = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testOptionalForce1() throws {
    opt!.i = switch Bool.random() { case true: throw Err() case false: 0 }
  }

  mutating func testOptionalForce2() throws {
    opt!.computed = switch Bool.random() { case true: throw Err() case false: 0 }
  }

  mutating func testOptionalForce3(_ g: G) throws {
    opt!.coroutined = switch g { case .e(let i): i default: throw Err() }
  }

  mutating func testOptionalForce4() throws {
    optopt!!.i = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testOptionalForce5() throws {
    optopt!!.opt = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testOptionalForce6() throws {
    optopt!!.opt! = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testSubscript1() throws {
    s[5] = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testSubscript2() throws {
    opt?[5] = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testSubscript3() throws {
    opt![5] = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testKeyPath1(_ kp: WritableKeyPath<S, Int>) throws {
    s[keyPath: kp] = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testKeyPath2(_ kp: WritableKeyPath<S, Int>) throws {
    opt?[keyPath: kp] = switch Bool.random() { case true: 1 case false: throw Err() }
  }

  mutating func testKeyPath3(_ kp: WritableKeyPath<S, Int>) throws {
    opt![keyPath: kp] = switch Bool.random() { case true: 1 case false: throw Err() }
  }
}

func exprPatternInClosure() {
  let f: (Int) -> Void = { i in
    switch i {
    case i:
      ()
    default:
      ()
    }
  }
}

func testNeverSwitch1() {
  let x = switch fatalError() {}
  return x
}

func testNeverSwitch2() -> Never {
  let x = switch fatalError() {
            case let x: x
          }
  return x
}

func testNeverSwitch3() -> Int {
  let x = switch fatalError() {
            case fatalError(): 0
            case _ where .random(): 1
            default: 2
          }
  return x
}

func testNeverSwitch4() {
  let x: Void
  x = switch fatalError() {}
  return x
}

func testNeverSwitch5() -> Never {
  let x: Never
  x = switch fatalError() {
        case let x: x
      }
  return x
}

func testNeverSwitch6() -> Int {
  let x: Int
  x = switch fatalError() {
        case fatalError(): 0
        case _ where .random(): 1
        default: 2
      }
  return x
}

func testNeverSwitch7() {
  let _ = switch fatalError() {}
  let _ = switch fatalError() { case let x: x }
  let _ = switch fatalError() { default: "" }
}

func testNeverSwitch8() {
  let _ = switch fatalError() { default: C() }
}

func testNeverSwitch9() {
  let i = switch Bool.random() {
  case true:
    switch fatalError() {}
  case false:
    switch fatalError() {}
  }
  return i
}

func testNeverSwitch10() -> Never {
  switch fatalError() {}
}

func testNeverSwitch11() {
  return switch fatalError() {}
}

func testNeverSwitch12() -> Never {
  return switch fatalError() { case let x: x }
}

func testNeverSwitch13() {
  return switch fatalError() { case let x: x }
}

extension Never {
  init(value: Self) {
    self = switch value { case let v: v }
  }
}

func testNever5() -> Int {
  switch Bool.random() {
  case true:
    fatalError()
  case false:
    0
  }
}
// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr10testNever5SiyF : $@convention(thin) () -> Int
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       switch_value {{.*}}, case {{%[0-9]+}}: [[BB_TRUE:bb[0-9]+]], case {{%[0-9]+}}: [[BB_FALSE:bb[0-9]+]]
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

func testNever6() -> Int {
  switch Bool.random() {
  case true:
    never()
  case false:
    0
  }
}

func neverTuple() -> (Never, Int) { fatalError() }

func testNever7() -> (Never, Int) {
  switch Bool.random() {
  case true:
    neverTuple()
  case false:
    (never(), 0)
  }
}
// CHECK-LABEL: sil hidden [ossa] @$s11switch_expr10testNever7s5NeverO_SityF : $@convention(thin) () -> (Never, Int)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $(Never, Int)
// CHECK:       switch_value {{.*}}, case {{%[0-9]+}}: [[BB_TRUE:bb[0-9]+]], case {{%[0-9]+}}: [[BB_FALSE:bb[0-9]+]]
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
  let fn = { [x = switch Bool.random() { case true: 0 case false: 1 }] in x }
  return fn()
}

// https://github.com/apple/swift/issues/68764
func testConditionalCast<T>(_ x: Any, _ y: Int) -> T? {
  switch y {
  default:
    x as? T
  }
}

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
}

// rdar://119158202 - Make sure we correctly contextualize local bindings.
func testLazyLocal(_ x: Int?) {
  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1aL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Optional<Int>) -> Int
  lazy var a = switch x { case let x?: x case nil: 0 }
  _ = a

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1bL_SSvg : $@convention(thin) (@guaranteed { var Optional<String> }) -> @owned String
  lazy var b = switch Bool.random() {
  case true:
    let x = ""
    then x
  case false:
    ""
  }
  _ = b

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1cL_SSvg : $@convention(thin) (@guaranteed { var Optional<String> }) -> @owned String
  lazy var c = switch Bool.random() {
  case true:
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1cL_SSvg1xL3_SSvg : $@convention(thin) (@guaranteed { var Optional<String> }) -> @owned String
    lazy var x = ""
    then x
  case false:
    ""
  }
  _ = c

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1dL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Optional<Int>) -> Int
  lazy var d = switch Bool.random() {
  case true:
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1dL_Sivg1yL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Optional<Int>) -> Int
    lazy var y = switch x { case let x?: x case nil: 0 }
    then y
  case false:
    0
  }
  _ = d

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1eL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int
  lazy var e = switch Bool.random() {
  case true:
    @Wrapper
    var x = 0
    then x
  case false:
    0
  }
  _ = e

  // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr13testLazyLocalyySiSgF1fL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int
  lazy var f = switch 0 {
  case 1:
    1
  default:
    0
  }
  _ = f
}

struct LazyProp {
  var a: Int?

  // CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s11switch_expr8LazyPropV1bSivg : $@convention(method) (@inout LazyProp) -> Int
  lazy var b = switch a { case let a?: a case nil: 0 }

  // CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s11switch_expr8LazyPropV1cSivg : $@convention(method) (@inout LazyProp) -> Int
  lazy var c = switch Bool.random() {
  case true:
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr8LazyPropV1cSivg1xL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int
    lazy var x = 0
    then x
  case false:
    0
  }

  // CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s11switch_expr8LazyPropV1dSivg : $@convention(method) (@inout LazyProp) -> Int
  lazy var d = switch Bool.random() {
  case true:
    // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11switch_expr8LazyPropV1dSivg1xL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, @inout_aliasable LazyProp) -> Int
    lazy var x = switch a { case let a?: a case nil: 0 }
    then x
  case false:
    0
  }
}
