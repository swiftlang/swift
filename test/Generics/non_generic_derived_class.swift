// RUN: %target-parse-verify-swift

class Base<T> {
  class func f(arg: T) -> Int {
    return 0
  }
}

class Derived : Base<Int> {}

var a = Derived.f(42)
