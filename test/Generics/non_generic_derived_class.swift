// RUN: %target-parse-verify-swift

class Base<T> {
  class func f(arg: T) -> Int {
    return 0
  }
}

class Derived : Base<Int> {} // expected-error{{classes derived from generic classes must also be generic}}

var a = Derived.f(42)
