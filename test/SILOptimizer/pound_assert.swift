// RUN: %target-swift-frontend -emit-sil %s -verify

func isOne(_ x: Int) -> Bool {
  return x == 1
}

func assertionSuccess() {
  #assert(isOne(1))
  #assert(isOne(1), "1 is not 1")
}

func assertionFailure() {
  #assert(isOne(2)) // expected-error{{assertion failed}}
  #assert(isOne(2), "2 is not 1") // expected-error{{2 is not 1}}
}

func nonConstant() {
  #assert(isOne(Int(readLine()!)!)) // expected-error{{#assert condition not constant}}
  #assert(isOne(Int(readLine()!)!), "input is not 1") // expected-error{{#assert condition not constant}}
}



func recursive(a: Int) -> Int {
  if a == 0 { return 0 }     // expected-note {{expression is too large to evaluate at compile-time}}
  return recursive(a: a-1)
}

func recursion() {
  // expected-error @+1 {{#assert condition not constant}}
  #assert(recursive(a: 20000) > 42)
}

