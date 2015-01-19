// RUN: %target-parse-verify-swift

func makeIncrementor(amount: Int) -> () -> Int {
   func incrementor() -> Int {
      currentTotal += amount // expected-error{{cannot capture 'currentTotal' before it is declared}}
      return currentTotal // note: redundant diagnostic suppressed
   }
   var currentTotal = 0 // expected-note{{'currentTotal' declared here}}
   return incrementor
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
  { [unowned x] in x.foo() }()
}
