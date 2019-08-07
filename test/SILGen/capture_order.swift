// RUN: %target-swift-emit-silgen %s -verify

/// We emit an invalid forward capture as an 'undef'; make sure
/// we cover the various possible cases.

public func captureBeforeDefLet(amount: Int) -> () -> Int {
  func getter() -> Int { // expected-error {{closure captures 'modifiedAmount' before it is declared}}
    return modifiedAmount // expected-note {{captured here}}
  }
  let closure = getter
  let modifiedAmount = amount // expected-note{{captured value declared here}}
  // FIXME: Bogus warning!
  // expected-warning@-2 {{initialization of immutable value 'modifiedAmount' was never used; consider replacing with assignment to '_' or removing it}}
  return closure
}

public func captureBeforeDefVar(amount: Int) -> () -> Int {
  func incrementor() -> Int { // expected-error {{closure captures 'currentTotal' before it is declared}}
    currentTotal += amount // expected-note {{captured here}}
    return currentTotal
  }
  let closure = incrementor
  var currentTotal = 0 // expected-note{{captured value declared here}}
  // FIXME: Bogus warning!
  // expected-warning@-2 {{variable 'currentTotal' was written to, but never read}}
  currentTotal = 1
  return closure
}

public func captureBeforeDefWeakVar(obj: AnyObject) -> () -> AnyObject? {
  func getter() -> AnyObject? { // expected-error {{closure captures 'weakObj' before it is declared}}
    return weakObj // expected-note {{captured here}}
  }
  let closure = getter
  weak var weakObj: AnyObject? = obj // expected-note{{captured value declared here}}
  // FIXME: Bogus warning!
  // expected-warning@-2 {{variable 'weakObj' was written to, but never read}}
  return closure
}

public func captureBeforeDefUnownedLet(obj: AnyObject) -> () -> AnyObject? {
  func getter() -> AnyObject? { // expected-error {{closure captures 'unownedObj' before it is declared}}
    return unownedObj // expected-note {{captured here}}
  }
  let closure = getter
  unowned let unownedObj: AnyObject = obj // expected-note{{captured value declared here}}
  // FIXME: Bogus warning!
  // expected-warning@-2 {{immutable value 'unownedObj' was never used; consider replacing with '_' or removing it}}
  return closure
}

public func captureBeforeDefUnownedVar(obj: AnyObject) -> () -> AnyObject? {
  func getter() -> AnyObject? { // expected-error {{closure captures 'unownedObj' before it is declared}}
    return unownedObj // expected-note {{captured here}}
  }
  let closure = getter
  unowned var unownedObj: AnyObject = obj // expected-note{{captured value declared here}}
  // FIXME: Bogus warning!
  // expected-warning@-2 {{variable 'unownedObj' was never used; consider replacing with '_' or removing it}}
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
  // FIXME: Bogus warning!
  // expected-warning@-2 {{initialization of variable 'currentTotal' was never used; consider replacing with assignment to '_' or removing it}}

  _ = x
}

/// Regression tests

func sr3210_crash() {
  defer { // expected-error {{'defer' block captures 'b' before it is declared}}
    print("\(b)") // expected-note {{captured here}}
  }

  return

  let b = 2 // expected-note {{captured value declared here}}
  // expected-warning@-1 {{code after 'return' will never be executed}}
  // FIXME: Bogus warning!
  // expected-warning@-3 {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
}

func sr3210() {
  defer {
    print("\(b)")
  }

  let b = 2
  // FIXME: Bogus warning!
  // expected-warning@-2 {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
}

class SR4812 {
  public func foo() {
    let bar = { [weak self] in
    // expected-error@-1 {{closure captures 'bar' before it is declared}}
    // expected-note@-2 {{captured value declared here}}
    // expected-warning@-3 {{variable 'self' was written to, but never read}}
      bar2()
    }
    func bar2() {
      bar() // expected-note {{captured here}}
    }
    bar()
  }
}

func timeout(_ f: @escaping () -> Void) {
  f()
}

func sr10687() {
  timeout { // expected-error {{closure captures 'x' before it is declared}}
    proc()
  }
  
  let x = 0 // expected-note {{captured value declared here}}
  
  func proc() {
    _ = x // expected-note {{captured here}}
  }
}

class rdar40600800 {
  func foo() {
    let callback = { // expected-error {{closure captures 'callback' before it is declared}}
    // expected-note@-1 {{captured value declared here}}
      innerFunction()
    }

    func innerFunction() {
      let closure = { // expected-note {{captured here}}
      // FIXME: Bogus warning!
      // expected-warning@-2 {{initialization of immutable value 'closure' was never used; consider replacing with assignment to '_' or removing it}}
        callback()
      }
    }
  }
}
