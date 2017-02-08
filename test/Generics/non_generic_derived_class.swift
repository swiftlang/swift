// RUN: %target-typecheck-verify-swift

class Base<T> {
  class func f(_ arg: T) -> Int {
    return 0
  }
}

class Derived : Base<Int> {}

var a = Derived.f(42)
