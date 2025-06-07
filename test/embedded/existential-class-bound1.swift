// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

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
  final var t: T

  init(t : T) { self.t = t }

  public func bar() {
    print("bar")
  }
}

public class DerivedFromGenClass2: GenClass2<Int> {
  init() { super.init(t: 42) }

  public override func bar() {
    print("derived-bar")
  }
}

final public class GenClass3<V>: ProtoWithAssocConf {
  public func foo() -> GenClass2<Int> {
    print("foo")
    return GenClass2(t: 27)
  }
}

final public class OtherClass: ProtoWithAssocConf {
  public func foo() -> GenClass2<Int> {
    print("other-foo")
    return DerivedFromGenClass2()
  }
}


public func createExWithAssocConf() -> any ProtoWithAssocConf {
  return GenClass3<Int>()
}

public func callExWithAssocConf(_ p: any ProtoWithAssocConf) {
  let x = p.foo()
  x.bar()
}

public class Base<T>: ClassBound {
  public func foo() { print("Base.foo()") }
  public func bar() { print("Base.bar()") }
}

public class Derived1: Base<Int> {
  public override func foo() { print("Derived1.foo()") }
  public override func bar() { print("Derived1.bar()") }
}

public class Derived2<T>: Base<T> {
  public override func foo() { print("Derived2.foo()") }
  public override func bar() { print("Derived2.bar()") }
}

public func takes_p1(_ p: P1) {
  p.normal()
}

public protocol P1: AnyObject {
  func normal()
}

public protocol P2 {
  func foo()
}

public class ConditionalConformanceBase<A> {
  final var a: A

  init(a: A) { self.a = a }
}

extension ConditionalConformanceBase: P1 where A: P2 {
  public func normal() {
    a.foo()
  }
}

public class ConditionalConformanceDerived<T>: ConditionalConformanceBase<T> {
  init(t: T) { super.init(a: t) }
}


public func testConditionalConformance<T: P2>(t: T) {
  takes_p1(ConditionalConformanceDerived(t: t))
}


struct S: P2 {
  var i: Int

  func foo() {
    print(i)
  }
}

protocol Q3 {
  func bar()
}

protocol P3<T>: AnyObject {
  associatedtype T: Q3

  var t: T { get }

  func foo()
}

extension P3 {
  func foo() {
    t.bar()
  }
}

final class C3<T: Q3>: P3 {
  var t: T


  init(t: T) { self.t = t }
}

struct S3<I: BinaryInteger>: Q3 {
  var x: I

  func bar() {
    print(x)
  }
}

@inline(never)
func testP3() -> any P3 {
  return C3(t: S3(x: 102))
}

protocol P4<T>: AnyObject {
  associatedtype T: Q

  var t: T { get }

  func foo()
}

extension P4 {
  func foo() {
    t.bar()
  }
}

final class C4<T: Q>: P4 {
  var t: T


  init(t: T) { self.t = t }
}

class K4<I: BinaryInteger>: Q {
  var x: I

  init(x: I) { self.x = x }

  func bar() {
    print(x)
  }
}

@inline(never)
func testP4() -> any P4 {
  return C4(t: K4(x: 437))
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
        callExWithAssocConf(OtherClass())
        // CHECK: other-foo
        // CHECK: derived-bar
        test(existential: Derived1())
        // CHECK: Derived1.foo()
        // CHECK: Derived1.bar()
        test(existential: Derived2<Bool>())
        // CHECK: Derived2.foo()
        // CHECK: Derived2.bar()
        testConditionalConformance(t: S(i: 27))
        // CHECK: 27
        testP3().foo()
        // CHECK: 102
        testP4().foo()
        // CHECK: 437
    }
}

