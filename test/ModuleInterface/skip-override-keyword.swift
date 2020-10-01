// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface %s
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.swiftmodule

// RUN: %target-swift-frontend -typecheck -enable-testing -module-name FooWithTesting -emit-module-interface-path %t/FooWithTesting.swiftinterface %s
// RUN: %target-swift-frontend -compile-module-from-interface %t/FooWithTesting.swiftinterface -o %t/FooWithTesting.swiftmodule

public class BaseClass {
  init() { }
  var property: Int { return 1 }
  func doSomething() { }
  subscript(index: Int) -> Int { get { return 0 } set(newValue) {} }
  @usableFromInline func doSomethingInline() {}
  @usableFromInline func doSomethingUsableFromInline() {}
}

public class DerivedClass: BaseClass {
  public override init() { super.init() }
  public override var property : Int { return 0 }
  public override func doSomething() { }
  public override subscript(index: Int) -> Int { get {return 0} set(newValue) {} }
  @inlinable public override func doSomethingInline() { super.doSomethingInline() }
  @usableFromInline override func doSomethingUsableFromInline() { super.doSomethingUsableFromInline() }
}
