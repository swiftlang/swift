// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/TestResilient.swiftinterface) %s -module-name TestResilient
// RUN: %target-swift-typecheck-module-from-interface(%t/TestResilient.swiftinterface) -module-name TestResilient
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface -swift-version 5 %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -swift-version 5  -emit-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s

@propertyWrapper
public struct Wrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

@propertyWrapper
public struct WrapperWithInitialValue<T> {
  public var wrappedValue: T

  public init(initialValue: T) {
    self.wrappedValue = initialValue
  }

  public init(alternate value: T) {
    self.wrappedValue = value
  }

  public var projectedValue: Wrapper<T> {
    get { Wrapper(wrappedValue: wrappedValue) }
    set { wrappedValue = newValue.wrappedValue }
  }
}

@propertyWrapper
public struct ProjectedValueWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  public init(projectedValue: Wrapper<T>) {
    self.wrappedValue = projectedValue.wrappedValue
  }

  public var projectedValue: Wrapper<T> {
    get { Wrapper(wrappedValue: wrappedValue) }
    set { wrappedValue = newValue.wrappedValue }
  }
}

// CHECK: public struct HasWrappers {
public struct HasWrappers {
  // CHECK: @TestResilient.Wrapper public var x: {{(Swift.)?}}Int {
  // CHECK-NEXT: get
  // CHECK-NEXT: set
  // CHECK-NEXT: _modify  
  // CHECK-NEXT: }  
  @Wrapper public var x: Int

  // CHECK: @TestResilient.WrapperWithInitialValue @_projectedValueProperty($y) public var y: Swift.Int {
  // CHECK-NEXT: get
  // CHECK-NEXT: }  
  @WrapperWithInitialValue public private(set) var y = 17

  // CHECK: public var $y: TestResilient.Wrapper<{{(Swift.)?}}Int> {
  // CHECK-NEXT: get
  // CHECK-NEXT: }  

  // CHECK: @TestResilient.WrapperWithInitialValue @_projectedValueProperty($z) public var z: Swift.Bool {
  // CHECK-NEXT: get
  // CHECK-NEXT: set
  // CHECK-NEXT: _modify
  // CHECK-NEXT: }  
  @WrapperWithInitialValue(alternate: false) public var z

  // CHECK: public func hasParameterWithImplementationWrapper(x: Swift.Int)
  public func hasParameterWithImplementationWrapper(@Wrapper x: Int) { }

  // CHECK: public func hasParameterWithImplementationWrapperComposed(x: Swift.Int)
  public func hasParameterWithImplementationWrapperComposed(@Wrapper @ProjectedValueWrapper x: Int) { }

  // CHECK: @inlinable public func hasParameterWithImplementationWrapperInlinable(@TestResilient.Wrapper x: Swift.Int)
  @inlinable public func hasParameterWithImplementationWrapperInlinable(@Wrapper x: Int) { }

  // CHECK: @_alwaysEmitIntoClient public func hasParameterWithImplementationWrapperAEIC(@TestResilient.Wrapper x: Swift.Int)
  @_alwaysEmitIntoClient public func hasParameterWithImplementationWrapperAEIC(@Wrapper x: Int) { }

  // CHECK: public func hasParameterWithAPIWrapper(@TestResilient.ProjectedValueWrapper x: Swift.Int)
  public func hasParameterWithAPIWrapper(@ProjectedValueWrapper x: Int) { }

  // CHECK: public func hasParameterWithAPIWrapperComposed(@TestResilient.ProjectedValueWrapper @TestResilient.Wrapper x: Swift.Int)
  public func hasParameterWithAPIWrapperComposed(@ProjectedValueWrapper @Wrapper x: Int) { }
}
