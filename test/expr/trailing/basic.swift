// RUN: %swift -parse %s -verify

func takeFunc(f : (Int) -> Int) -> Int {}
func takeValueAndFunc(value : Int, f : (Int) -> Int) {}
func takeTwoFuncs(f : (Int) -> Int, g : (Int) -> Int) {}

struct X {
  func takeFunc(f : (Int) -> Int) {}
  func takeValueAndFunc(value : Int, f : (Int) -> Int) {}
  func takeTwoFuncs(f : (Int) -> Int, g : (Int) -> Int) {}
}

func addToMemberCalls(x : X) {
  x.takeFunc() { |x| x }
  x.takeFunc() { $0 }
  x.takeValueAndFunc(1) { |x| x }
  x.takeValueAndFunc(value : 1) { |x| x }
  x.takeTwoFuncs() { |x| x } { |y| y }
}

func addToCalls() {
  takeFunc() { |x| x }
  takeFunc() { $0 }
  takeValueAndFunc(1) { |x| x }
  takeValueAndFunc(value : 1) { |x| x }
  takeTwoFuncs() { |x| x } { |y| y }
}

func makeCalls() {
  takeFunc { |x| x }
  takeFunc { $0 }
  takeTwoFuncs { |x| x } { |y| y }
}

func notPostfix() {
  1 + takeFunc { $0 } // expected-error{{trailing closure can only follow a postfix expression}} expected-note{{complete expression}}{3-3:(}{14-4:)} expected-note{{last postfix}}{7-7:(}{21-21:)} expected-error{{does not type-check}}
}
