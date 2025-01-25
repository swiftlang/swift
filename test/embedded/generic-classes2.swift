// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

class B<T> {
}

class D<T>: B<T> {
}

func callee<T>(_ t: T.Type) {
  _ = D<T>()
}

public func test() {
  callee(Int.self)
  print("OK!")
}

public class Base<T: BinaryInteger> {
  public func foo(_ t: T) {
    print(t)
  }
}

public class Derived: Base<Int> {
}


func testNonGenericDerived(_ d: Derived) {
  d.foo(27)
}

public protocol P {
    func mask()
}

public struct ConcreteP: P {
  var x: Int
  public func mask() {
    print(x)
  }
}

class BaseClass<SomeP: P> {
  func foo(_ p: SomeP) {
    p.mask()
  }
}

final class SubClass<T, SomeP: P>: BaseClass<SomeP> {
}

public class BaseWithClassMethod<T: BinaryInteger> {
  class func foo(_ t: T) {
    print("BaseWithClassMethod")
    print(t)
  }
}

public class DerivedWithClassMethod<T: BinaryInteger> : BaseWithClassMethod<T> {
  override class func foo(_ t: T) {
    print("DerivedWithClassMethod")
    print(t)
  }
}

func testClassMethod() -> BaseWithClassMethod<Int>.Type {
  return DerivedWithClassMethod<Int>.self
}

@main
struct Main {
  static func main() {
    // CHECK: OK!
    test()

    // CHECK: 27
    testNonGenericDerived(Derived())

    let x = SubClass<Int, ConcreteP>()
    // CHECK: 42
    x.foo(ConcreteP(x: 42))

    let t = testClassMethod()
    // CHECK: 123
    t.foo(123)
  }
}

