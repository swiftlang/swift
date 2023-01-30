// RUN: %target-typecheck-verify-swift

class Base<T> {
  class func f(_ arg: T) -> Int {
    return 0
  }
}

class Derived : Base<Int> {}

var a = Derived.f(42)

// https://github.com/apple/swift/issues/51655

protocol EmptyProtocol {}
class AbstractFoobar<Foo> {}
// This used to cause the swift compiler to never finish compiling.
final class SomeFoobar: AbstractFoobar<SomeFoobar.Foo> {
    enum Foo: EmptyProtocol {}
}
