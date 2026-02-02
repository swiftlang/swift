// RUN: %target-swift-emit-silgen -enable-upcoming-feature ImmutableWeakCaptures %s -verify

// REQUIRES: swift_feature_ImmutableWeakCaptures

/// We emit an invalid forward capture as an 'undef'; make sure
/// we cover the various possible cases.

public func captureBeforeDefLet(amount: Int) -> () -> Int {
  func getter() -> Int { // expected-error {{closure captures 'modifiedAmount' before it is declared}}
    return modifiedAmount // expected-note {{captured here}}
  }
  let closure = getter
  let modifiedAmount = amount // expected-note{{captured value declared here}}
  return closure
}

public func captureBeforeDefVar(amount: Int) -> () -> Int {
  func incrementor() -> Int { // expected-error {{closure captures 'currentTotal' before it is declared}}
    currentTotal += amount // expected-note {{captured here}}
    return currentTotal
  }
  let closure = incrementor
  var currentTotal = 0 // expected-note{{captured value declared here}}
  currentTotal = 1
  return closure
}

public func captureBeforeDefWeakVar(obj: AnyObject) -> () -> AnyObject? {
  func getter() -> AnyObject? { // expected-error {{closure captures 'weakObj' before it is declared}}
    return weakObj // expected-note {{captured here}}
  }
  let closure = getter
  weak let weakObj: AnyObject? = obj // expected-note{{captured value declared here}}
  return closure
}

public func captureBeforeDefUnownedLet(obj: AnyObject) -> () -> AnyObject? {
  func getter() -> AnyObject? { // expected-error {{closure captures 'unownedObj' before it is declared}}
    return unownedObj // expected-note {{captured here}}
  }
  let closure = getter
  unowned let unownedObj: AnyObject = obj // expected-note{{captured value declared here}}
  return closure
}

public func captureBeforeDefUnownedVar(obj: AnyObject) -> () -> AnyObject? {
  func getter() -> AnyObject? { // expected-error {{closure captures 'unownedObj' before it is declared}}
    return unownedObj // expected-note {{captured here}}
  }
  let closure = getter
  unowned var unownedObj: AnyObject = obj // expected-note{{captured value declared here}}
  // expected-warning@-1 {{variable 'unownedObj' was never mutated; consider changing to 'let' constant}}
  return closure
}

/// Examples of transitive capture

func pingpong() {
  func ping() -> Int {
    return pong()
  }
  func pong() -> Int {
    return ping()
  }
  _ = ping()
}

func transitiveForwardCapture() {
  func ping() -> Int { // expected-error {{closure captures 'x' before it is declared}}
    return pong()
  }
  _ = ping()
  var x = 1 // expected-note {{captured value declared here}}
  func pong() -> Int {
    x += 1 // expected-note {{captured here}}
    return ping()
  }
}

func transitiveForwardCapture2() {
  func ping() -> Int { // expected-error {{closure captures 'x' before it is declared}}
    _ = pong()
  }
  _ = ping()
  var x = 1 // expected-note {{captured value declared here}}
  func pong() -> Int {
    _ = pung()
  }
  func pung() -> Int {
    x += 1 // expected-note {{captured here}}
    return ping()
  }
}

func transitiveForwardCapture3() {
  var y = 2
  func ping() -> Int { // expected-error {{closure captures 'x' before it is declared}}
    _ = pong()
  }
  _ = ping()
  var x = 1 // expected-note {{captured value declared here}}
  func pung() -> Int {
    x += 1 // expected-note {{captured here}}
    return ping()
  }
  func pong() -> Int {
    y += 2
    _ = pung()
  }
}

func captureInClosure() {
  let x = { (i: Int) in // expected-error {{closure captures 'currentTotal' before it is declared}}
    currentTotal += i // expected-note {{captured here}}
  }

  var currentTotal = 0 // expected-note {{captured value declared here}}

  _ = x
}

/// Regression tests

// https://github.com/apple/swift/issues/47389
class С_47389 {
  public func foo() {
    let bar = { [weak self] in
    // expected-error@-1 {{closure captures 'bar' before it is declared}}
    // expected-note@-2 {{captured value declared here}}
    // expected-warning@-3 {{capture 'self' was never used}}
      bar2()
    }
    func bar2() {
      bar() // expected-note {{captured here}}
    }
    bar()
  }
}

// https://github.com/apple/swift/issues/53085
do {
  func timeout(_ f: @escaping () -> Void) {
    f()
  }

  func f() {
    timeout { // expected-error {{closure captures 'x' before it is declared}}
      proc()
    }

    let x = 0 // expected-note {{captured value declared here}}

    func proc() {
      _ = x // expected-note {{captured here}}
    }
  }
}

class rdar40600800 {
  func foo() {
    let callback = { // expected-error {{closure captures 'callback' before it is declared}}
    // expected-note@-1 {{captured value declared here}}
      innerFunction()
    }

    func innerFunction() {
      let closure = {
      // expected-warning@-1 {{initialization of immutable value 'closure' was never used; consider replacing with assignment to '_' or removing it}}
        callback() // expected-note {{captured here}}
      }
    }
  }
}

// https://github.com/apple/swift/issues/57097
// Make sure we can't capture an uninitialized 'var' box, either.
func f_57097() {
  func g() -> Int { // expected-error {{closure captures 'r' before it is declared}}
    _ = r // expected-note {{captured here}}
    return 5
  }
  var r = g() // expected-note {{captured value declared here}}
  // expected-warning@-1 {{variable 'r' was never mutated; consider changing to 'let' constant}}
}

class class77933460 {}

func func77933460() {
  var obj: class77933460 = { obj }()
  // expected-error@-1 {{closure captures 'obj' before it is declared}}
  // expected-note@-2 {{captured here}}
  // expected-note@-3 {{captured value declared here}}
  // expected-warning@-4 {{variable 'obj' was never mutated; consider changing to 'let' constant}}
}

// MARK: - Forward Declared Lets

// https://github.com/swiftlang/swift/issues/84909
// Make sure we can't capture an uninitialized 'let' temporary allocation either.

protocol P {}

enum E {
  static func static_gen_fwd<T>(_ g: () -> T) -> T { g() }
}

func global_fwd(_ a: () -> Any) -> Any { a() }
func global_gen_fwd<T>(_ g: () -> T) -> T { g() }
func global_fwd_p(_ p: () -> any P) -> any P { p() }

func forward_declared_let_captures() {
  do {
    let bad: Any = { bad }()
    // expected-error@-1 {{closure captures 'bad' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    func fwd(_ i: () -> Any) -> Any { i() }
    let bad = fwd { bad }
    // expected-error@-1 {{closure captures 'bad' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let bad = global_fwd { bad }
    // expected-error@-1 {{closure captures 'bad' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let bad: Any = global_gen_fwd { bad }
    // expected-error@-1 {{closure captures 'bad' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let bad: Any = E.static_gen_fwd { bad }
    // expected-error@-1 {{closure captures 'bad' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let badNested: Any = global_fwd { { [badNested] in badNested }() }
    // expected-error@-1 {{closure captures 'badNested' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let badOpt: Any? = { () -> Any? in badOpt }()
    // expected-error@-1 {{closure captures 'badOpt' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let badTup: (Any, Any) = { (badTup.0, badTup.1) }()
    // expected-error@-1 {{closure captures 'badTup' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let badTup: (Int, Any) = { (badTup.0, badTup.1) }()
    // expected-error@-1 {{closure captures 'badTup' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let (badTup3, badTup4): (Any, Any) = { (badTup4, badTup3) }()
    // expected-error@-1 {{closure captures 'badTup4' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
    // expected-error@-4 {{closure captures 'badTup3' before it is declared}}
    // expected-note@-5 {{captured here}}
    // expected-note@-6 {{captured value declared here}}
  }

  do {
    struct S { var p: Any }
    let badStruct: S = { S(p: badStruct.p) }()
    // expected-error@-1 {{closure captures 'badStruct' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    enum EE {
      case boring
      case weird(Any)
      case strange(Any)
    }

    let badEnum: EE = { .weird(EE.strange(badEnum)) }()
    // expected-error@-1 {{closure captures 'badEnum' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }

  do {
    let badproto: any P = global_fwd_p { badproto }
    // expected-error@-1 {{closure captures 'badproto' before it is declared}}
    // expected-note@-2 {{captured here}}
    // expected-note@-3 {{captured value declared here}}
  }
}

func forward_declared_let_captures_local_fn() {
  do {
    func bad_local_f() -> Any { bad }
    // expected-error@-1 {{closure captures 'bad' before it is declared}}
    // expected-note@-2 {{captured here}}
    let bad = bad_local_f()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func fwd(_ i: () -> Any) -> Any { i() }
    func badFn() -> Any {
      // expected-error@-1 {{closure captures 'bad' before it is declared}}
      fwd { bad }
      // expected-note@-1 {{captured here}}
    }
    let bad = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> Any {
      // expected-error@-1 {{closure captures 'bad' before it is declared}}
      global_gen_fwd { bad }
      // expected-note@-1 {{captured here}}
    }
    let bad = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> Any {
      // expected-error@-1 {{closure captures 'bad' before it is declared}}
      E.static_gen_fwd { bad }
      // expected-note@-1 {{captured here}}
    }
    let bad = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> Any {
      // expected-error@-1 {{closure captures 'badNested' before it is declared}}
      global_fwd { { [badNested] in badNested }() }
      // expected-note@-1 {{captured here}}
    }
    let badNested = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> Any? {
      // expected-error@-1 {{closure captures 'badOpt' before it is declared}}
      { () -> Any? in badOpt }()
      // expected-note@-1 {{captured here}}
    }
    let badOpt = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> (Any, Any) {
      // expected-error@-1 {{closure captures 'badTup' before it is declared}}
      { (badTup.0, badTup.1) }()
      // expected-note@-1 {{captured here}}
    }
    let badTup = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> (Int, Any) {
      // expected-error@-1 {{closure captures 'badTup' before it is declared}}
      { (badTup.0, badTup.1) }()
      // expected-note@-1 {{captured here}}
    }
    let badTup = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> (Any, Any) {
      // expected-error@-1 {{closure captures 'badTup3' before it is declared}}
      // expected-error@-2 {{closure captures 'badTup4' before it is declared}}
      { (badTup4, badTup3) }()
      // expected-note@-1 2{{captured here}}
    }
    let (badTup3, badTup4) = badFn()
    // expected-note@-1 2{{captured value declared here}}
  }

  do {
    struct S { var p: Any }
    func badFn() -> S {
      // expected-error@-1 {{closure captures 'badStruct' before it is declared}}
      { S(p: badStruct.p) }()
      // expected-note@-1 {{captured here}}
    }
    let badStruct = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    enum EE {
      case boring
      case weird(Any)
      case strange(Any)
    }

    func badFn() -> EE {
      // expected-error@-1 {{closure captures 'badEnum' before it is declared}}
      { .weird(EE.strange(badEnum)) }()
      // expected-note@-1 {{captured here}}
    }
    let badEnum = badFn()
    // expected-note@-1 {{captured value declared here}}
  }

  do {
    func badFn() -> any P {
      // expected-error@-1 {{closure captures 'badproto' before it is declared}}
      global_fwd_p { badproto }
      // expected-note@-1 {{captured here}}
    }
    let badproto = badFn()
    // expected-note@-1 {{captured value declared here}}
  }
}

func forward_declared_local_lazy_captures() {
  // runtime stack overflow
  lazy var infiniteRecurse: Any = { infiniteRecurse }()

  // function that returns itself
  lazy var hmm: () -> Any = { hmm }
}
