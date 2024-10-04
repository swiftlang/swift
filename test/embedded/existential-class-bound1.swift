// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

protocol ClassBound: AnyObject {
    func foo()
    func bar()
}

class MyClass {}
extension MyClass: ClassBound {
    func foo() { print("MyClass.foo()") }
    func bar() { print("MyClass.bar()") }
}

class MyOtherClass {}
extension MyOtherClass: ClassBound {
    func foo() { print("MyOtherClass.foo()") }
    func bar() { print("MyOtherClass.bar()") }
}

func test(existential: any ClassBound) {
    existential.foo()
    existential.bar()
}

public protocol ProtoWithAssocType<T>: AnyObject {
  associatedtype T
  func foo(t: T)
}

final public class GenClass<T: BinaryInteger>: ProtoWithAssocType {
  public func foo(t: T) {
    print(t)
  }
}

public func createExWithAssocType() -> any ProtoWithAssocType<Int> {
  return GenClass<Int>()
}

public func callExWithAssocType(_ p: any ProtoWithAssocType<Int>) {
  p.foo(t: 27)
}

public protocol Q: AnyObject {
  func bar()
}

public protocol ProtoWithAssocConf: AnyObject {
  associatedtype A: Q
  func foo() -> A
}

public class GenClass2<T>: Q {
  var t: T

  init(t : T) { self.t = t }

  public func bar() {
    print("bar")
  }
}

final public class GenClass3<V>: ProtoWithAssocConf {
  public func foo() -> GenClass2<Int> {
    print("foo")
    return GenClass2(t: 27)
  }
}


public func createExWithAssocConf() -> any ProtoWithAssocConf {
  return GenClass3<Int>()
}

public func callExWithAssocConf(_ p: any ProtoWithAssocConf) {
  let x = p.foo()
  x.bar()
}

@main
struct Main {
    static func main() {
        test(existential: MyClass())
        // CHECK: MyClass.foo()
        // CHECK: MyClass.bar()
        test(existential: MyOtherClass())
        // CHECK: MyOtherClass.foo()
        // CHECK: MyOtherClass.bar()
        callExWithAssocType(createExWithAssocType())
        // CHECK: 27
        callExWithAssocConf(createExWithAssocConf())
        // CHECK: foo
        // CHECK: bar
    }
}

