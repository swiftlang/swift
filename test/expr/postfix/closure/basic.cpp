// RUN: %swift -parse %s -verify

func takeFunc(f : (Int) -> Int) {}
func takeValueAndFunc(value : Int, f : (Int) -> Int) {}
func takeTwoFuncs(f : (Int) -> Int, g : (Int) -> Int) {}

struct X {
  func takeFunc(f : (Int) -> Int) {}
  func takeValueAndFunc(value : Int, f : (Int) -> Int) {}
  func takeTwoFuncs(f : (Int) -> Int, g : (Int) -> Int) {}
}

func addToMemberCalls(x : X) {
  x.takeFunc() { |x| x }
  x.takeValueAndFunc(1) { |x| x }
  x.takeValueAndFunc(value : 1) { |x| x }
  x.takeTwoFuncs() { |x| x } { |y| y }
}

func addToCalls() {
  takeFunc() { |x| x }
  takeValueAndFunc(1) { |x| x }
  takeValueAndFunc(value : 1) { |x| x }
  takeTwoFuncs() { |x| x } { |y| y }
}

func makeCalls() {
  takeFunc { |x| x }
  takeTwoFuncs { |x| x } { |y| y }
}

