// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Type-check function definitions
//===----------------------------------------------------------------------===//
protocol EqualComparable {
  func isEqual(other : This) -> Bool
}

func doCompare<T : EqualComparable, U : EqualComparable>(t1 : T, t2 : T, u : U) -> Bool {
  var b1 = t1.isEqual(t2)
  if (b1) {
    return true;
  }
  t1 = t2
  return t1.isEqual(u) // expected-error{{invalid conversion from type 'U' to 'T'}} // expected-note{{while converting}}
}

protocol MethodLessComparable {
  func isLess(other : This) -> Bool
}

func min<T : MethodLessComparable>(x : T, y : T) -> T {
  if (y.isLess(x)) { return y; }
  return x;
}
