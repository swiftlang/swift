// RUN: %swift -parse -verify -constraint-checker %s

protocol ConcatToAnything {
  func [infix] +++ <T>(lhs : This, other : T)
}

func min<T : Ordered>(x : T, y : T) -> T {
  if y < x { return y }
  return x
}

func weirdConcat<T : ConcatToAnything, U>(t : T, u : U) {
  t +++ u
  t +++ 1
  u +++ t // expected-error{{expression does not type-check}}
}

// Make sure that the protocol operators don't get in the way.
var b1, b2 : Bool
b1 != b2

extension Char {
  func isAlpha2() -> Bool {
    return (this >= 'A' && this <= 'Z') || (this >= 'a' && this <= 'z')
  }
}
