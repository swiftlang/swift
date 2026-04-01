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

// FIXME: Currently they crash SILGen (TypeConverter-setCaptureTypeExpansionContext-e72208.swift)
//func forward_declared_local_lazy_captures() {
//  // runtime stack overflow
//  var _infiniteRecurse: Any { infiniteRecurse }
//  lazy var infiniteRecurse = _infiniteRecurse
//
//  // function that returns itself
//  func _hmm() -> Any { hmm }
//  lazy var hmm = _hmm
//}
