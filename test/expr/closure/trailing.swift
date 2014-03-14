// RUN: %swift -parse %s -verify

func takeFunc(f: (Int) -> Int) -> Int {}
func takeValueAndFunc(value: Int, f: (Int) -> Int) {}
func takeTwoFuncs(f: (Int) -> Int, g: (Int) -> Int) {}

struct X {
  func takeFunc(f: (Int) -> Int) {}
  func takeValueAndFunc(value: Int, f: (Int) -> Int) {}
  func takeTwoFuncs(f: (Int) -> Int, g: (Int) -> Int) {}
}

func addToMemberCalls(x: X) {
  x.takeFunc() { x in x }
  x.takeFunc() { $0 }
  x.takeValueAndFunc(1) { x in x }
  x.takeValueAndFunc(value : 1) { x in x }
  x.takeTwoFuncs() { x in x } { y in y }
}

func addToCalls() {
  takeFunc() { x in x }
  takeFunc() { $0 }
  takeValueAndFunc(1) { x in x }
  takeValueAndFunc(value : 1) { x in x }
  takeTwoFuncs() { x in x } { y in y }
}

func makeCalls() {
  takeFunc { x in x }
  takeFunc { $0 }
  takeTwoFuncs { x in x } { y in y }
}

func notPostfix() {
  1 + takeFunc { $0 }
}

class C {
  func map(x: Int -> Int) -> C { return self }
  func filter(x: Int -> Bool) -> C { return self }
}

var a = C().map {$0 + 1}.filter {$0 % 3 == 0}

var b = C().map {$0 + 1}
  .filter {$0 % 3 == 0}
