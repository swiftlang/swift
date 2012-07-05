// RUN: %swift %s -parse -verify

struct S<T : FormattedPrintable> {
  var c : T
  static func f(a : T) -> T {
    return a
  }
  func f(a : T, b : Int) {
    return printf("%v %v %v", a, b, c)
  }
}
var a : S<Int>
a.f(1,2)
var b : Int = a.f(1)
