// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -module-name Foo
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// RUN: %target-swift-emit-module-interface(%t/FooWithTesting.swiftinterface) %s -module-name FooWithTesting -enable-testing
// RUN: %target-swift-typecheck-module-from-interface(%t/FooWithTesting.swiftinterface) -module-name FooWithTesting
// RUN: %FileCheck %s < %t/FooWithTesting.swiftinterface

// CHECK: @_hasMissingDesignatedInitializers public class BaseClass
public class BaseClass {
  init() { }
  var property: Int { return 1 }
  func doSomething() { }
  subscript(index: Int) -> Int { get { return 0 } set(newValue) {} }
  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func doSomethingInline()
  @usableFromInline func doSomethingInline() {}
  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func doSomethingUsableFromInline()
  @usableFromInline func doSomethingUsableFromInline() {}
}

// CHECK: @_inheritsConvenienceInitializers public class DerivedClass : {{Foo|FooWithTesting}}.BaseClass
public class DerivedClass: BaseClass {
  // CHECK: public init()
  public override init() { super.init() }
  // CHECK: public var property: Swift.Int
  public override var property : Int { return 0 }
  // CHECK: public func doSomething()
  public override func doSomething() { }
  // CHECK: public subscript(index: Swift.Int) -> Swift.Int
  public override subscript(index: Int) -> Int { get {return 0} set(newValue) {} }
  // CHECK: @inlinable override public func doSomethingInline() { super.doSomethingInline() }
  @inlinable public override func doSomethingInline() { super.doSomethingInline() }
  // CHECK: @usableFromInline
  // CHECK-NEXT: override internal func doSomethingUsableFromInline()
  @usableFromInline override func doSomethingUsableFromInline() { super.doSomethingUsableFromInline() }
}
