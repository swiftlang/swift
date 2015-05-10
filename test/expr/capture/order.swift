// RUN: %target-parse-verify-swift

func makeIncrementor(amount: Int) -> () -> Int {
  func incrementor() -> Int {
    currentTotal += amount // expected-error{{cannot capture 'currentTotal' before it is declared}}
    return currentTotal // note: redundant diagnostic suppressed
  }
  var currentTotal = 0 // expected-note{{'currentTotal' declared here}}
  return incrementor
}

func pingpong() {
  func ping() -> Int {
    return pong()
  }
  func pong() -> Int {
    return ping()
  }
  ping()
}

func transitiveForwardCapture() {
  func ping() -> Int {
    return pong() // expected-error{{cannot capture 'pong', which would use 'x' before it is declared}}
  }
  ping()
  var x = 1 // expected-note{{'x' declared here}}
  func pong() -> Int { // expected-note{{'pong', declared here, captures 'x'}}
    x += 1
    return ping()
  }
}

func transitiveForwardCapture2() {
  func ping() -> Int {
    pong() // expected-error{{cannot capture 'pong', which would use 'x' before it is declared}}
  }
  ping()
  var x = 1 // expected-note{{'x' declared here}}
  func pong() -> Int { // expected-note{{'pong', declared here, captures 'pung'}}
    pung()
  }
  func pung() -> Int { // expected-note{{'pung', declared here, captures 'x'}}
    x += 1
    return ping()
  }
}

func transitiveForwardCapture3() {
  var y = 2
  func ping() -> Int {
    pong() // expected-error{{cannot capture 'pong', which would use 'x' before it is declared}}
  }
  ping()
  var x = 1 // expected-note{{'x' declared here}}
  func pung() -> Int { // expected-note{{'pung', declared here, captures 'x'}}
    x += 1
    return ping()
  }
  func pong() -> Int { // expected-note{{'pong', declared here, captures 'pung'}}
    y += 2
    pung()
  }
}

func outOfOrderEnum() {
  func f() -> Suit {
    return .Club
  }

  enum Suit {
  case Club
  case Diamond
  case Heart
  case Spade
  }
}

func captureInClosure() {
  var x = { (i: Int) in 
    currentTotal += i // expected-error{{use of local variable 'currentTotal' before its declaration}}
  }

  var currentTotal = 0 // expected-note{{'currentTotal' declared here}}
}

class X { 
  func foo() { }
}

func captureLists(x: X) {
  { [unowned x] in x.foo() }();
  let _: Void = { [unowned x] in x.foo() }()
  let _: Void = { [weak x] in x?.foo() }()
}
