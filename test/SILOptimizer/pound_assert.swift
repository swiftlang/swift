// RUN: %target-swift-frontend -emit-sil %s -verify

func isOne(_ x: Int) -> Bool {
  return x == 1
}

func assertionSuccess() {
  #assert(isOne(1))
}

func assertionFailure() {
  #assert(isOne(2)) // expected-error{{assertion failed}}
}

func nonConstant() {
  #assert(isOne(Int(readLine()!)!)) // expected-error{{#assert condition not constant}}
}
