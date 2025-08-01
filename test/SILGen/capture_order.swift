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
