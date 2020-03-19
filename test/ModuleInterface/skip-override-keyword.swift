// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface %s
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.swiftmodule

public class BaseClass {
  var property: Int { return 1 }
  func doSomething() { }
  subscript(index: Int) -> Int { get { return 0 } set(newValue) {} }
}
public class DerivedClass: BaseClass {
  public override var property : Int { return 0 }
  public override func doSomething() { }
  public override subscript(index: Int) -> Int { get {return 0} set(newValue) {} }
}
