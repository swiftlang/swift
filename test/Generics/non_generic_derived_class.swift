// RUN: %target-typecheck-verify-swift

class Base<T> {
  class func f(_ arg: T) -> Int {
    return 0
  }
}

class Derived : Base<Int> {}

var a = Derived.f(42)

protocol SR9160_EmptyProtocol {}
class SR9160_AbstractFoobar<Foo> {}
// This used to cause the swift compiler to never finish compiling.
final class SR9160_SomeFoobar: SR9160_AbstractFoobar<SR9160_SomeFoobar.Foo> {
    enum Foo: SR9160_EmptyProtocol {}
}
