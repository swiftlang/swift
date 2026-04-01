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

@propertyWrapper public struct HasTwoTypeParameters<Value, Other> {
  public var wrappedValue: Value

  init(wrappedValue: Value, _ other: Other) {
    fatalError()
  }
}

// CHECK: public struct HasWrappers {
public struct HasWrappers {
<<<<<<< HEAD
  // CHECK: @TestResilient.Wrapper<Swift.Int> public var x: {{(Swift.)?}}Int {
=======
  // CHECK: @TestResilient::Wrapper<Swift::Int> public var x: {{(Swift::)?}}Int {
>>>>>>> origin/main
  // CHECK-NEXT: get
  // CHECK-NEXT: set
  // CHECK-NEXT: _modify  
  // CHECK-NEXT: }  
  @Wrapper public var x: Int

<<<<<<< HEAD
  // CHECK: @TestResilient.WrapperWithInitialValue<Swift.Int> @_projectedValueProperty($y) public var y: Swift.Int {
=======
  // CHECK: @TestResilient::WrapperWithInitialValue<Swift::Int> @_projectedValueProperty($y) public var y: Swift::Int {
>>>>>>> origin/main
  // CHECK-NEXT: get
  // CHECK-NEXT: }  
  @WrapperWithInitialValue public private(set) var y = 17

  // CHECK: public var $y: TestResilient::Wrapper<{{(Swift::)?}}Int> {
  // CHECK-NEXT: get
  // CHECK-NEXT: }  

<<<<<<< HEAD
  // CHECK: @TestResilient.HasTwoTypeParameters<Swift.Int, Swift.String> public var w1: Swift.Int {
=======
  // CHECK: @TestResilient::HasTwoTypeParameters<Swift::Int, Swift::String> public var w1: Swift::Int {
>>>>>>> origin/main
  // CHECK-NEXT:   get
  // CHECK-NEXT:   set
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  @HasTwoTypeParameters(wrappedValue: 0, "other") public var w1: Int

<<<<<<< HEAD
  // CHECK: @TestResilient.HasTwoTypeParameters<Swift.Int, Swift.String> public var w2: Swift.Int {
=======
  // CHECK: @TestResilient::HasTwoTypeParameters<Swift::Int, Swift::String> public var w2: Swift::Int {
>>>>>>> origin/main
  // CHECK-NEXT:   get
  // CHECK-NEXT:   set
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  @HasTwoTypeParameters("other") public var w2: Int = 0

<<<<<<< HEAD
  // CHECK: @TestResilient.HasTwoTypeParameters<TestResilient.HasTwoTypeParameters<Swift.Int, Swift.Double>, Swift.String> @TestResilient.HasTwoTypeParameters public var w3: Swift.Int {
=======
  // CHECK: @TestResilient::HasTwoTypeParameters<TestResilient::HasTwoTypeParameters<Swift::Int, Swift::Double>, Swift::String> @TestResilient::HasTwoTypeParameters public var w3: Swift::Int {
>>>>>>> origin/main
  // CHECK-NEXT:   get
  // CHECK-NEXT:   set
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  @HasTwoTypeParameters("a")  @HasTwoTypeParameters(1.0) public var w3: Int = 0

<<<<<<< HEAD
  // CHECK: @TestResilient.WrapperWithInitialValue<Swift.Bool> @_projectedValueProperty($z) public var z: Swift.Bool {
=======
  // CHECK: @TestResilient::WrapperWithInitialValue<Swift::Bool> @_projectedValueProperty($z) public var z: Swift::Bool {
>>>>>>> origin/main
  // CHECK-NEXT: get
  // CHECK-NEXT: set
  // CHECK-NEXT: _modify
  // CHECK-NEXT: }  
  @WrapperWithInitialValue(alternate: false) public var z

<<<<<<< HEAD
  // CHECK: @TestResilient.HasTwoTypeParameters<TestResilient.Wrapper<Swift.Int>, Swift.String> @TestResilient.Wrapper public var composed: Swift.Int {
=======
  // CHECK: @TestResilient::HasTwoTypeParameters<TestResilient::Wrapper<Swift::Int>, Swift::String> @TestResilient::Wrapper public var composed: Swift::Int {
>>>>>>> origin/main
  // CHECK-NEXT:   get
  // CHECK-NEXT:   set
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  @HasTwoTypeParameters("other") @Wrapper public var composed: Int = 42
  
<<<<<<< HEAD
  // CHECK: public func hasParameterWithImplementationWrapper(x: Swift.Int)
=======
  // CHECK: public func hasParameterWithImplementationWrapper(x: Swift::Int)
>>>>>>> origin/main
  public func hasParameterWithImplementationWrapper(@Wrapper x: Int) { }

  // CHECK: public func hasParameterWithImplementationWrapperComposed(x: Swift::Int)
  public func hasParameterWithImplementationWrapperComposed(@Wrapper @ProjectedValueWrapper x: Int) { }

  // CHECK: @inlinable public func hasParameterWithImplementationWrapperInlinable(@TestResilient::Wrapper x: Swift::Int)
  @inlinable public func hasParameterWithImplementationWrapperInlinable(@Wrapper x: Int) { }

  // CHECK: @_alwaysEmitIntoClient public func hasParameterWithImplementationWrapperAEIC(@TestResilient::Wrapper x: Swift::Int)
  @_alwaysEmitIntoClient public func hasParameterWithImplementationWrapperAEIC(@Wrapper x: Int) { }

<<<<<<< HEAD
  // CHECK: public func hasParameterWithAPIWrapper(@TestResilient.ProjectedValueWrapper<Swift.Int> x: Swift.Int)
  public func hasParameterWithAPIWrapper(@ProjectedValueWrapper x: Int) { }

  // CHECK: public func hasParameterWithAPIWrapperComposed(@TestResilient.ProjectedValueWrapper<TestResilient.Wrapper<Swift.Int>> @TestResilient.Wrapper x: Swift.Int)
=======
  // CHECK: public func hasParameterWithAPIWrapper(@TestResilient::ProjectedValueWrapper<Swift::Int> x: Swift::Int)
  public func hasParameterWithAPIWrapper(@ProjectedValueWrapper x: Int) { }

  // CHECK: public func hasParameterWithAPIWrapperComposed(@TestResilient::ProjectedValueWrapper<TestResilient::Wrapper<Swift::Int>> @TestResilient::Wrapper x: Swift::Int)
>>>>>>> origin/main
  public func hasParameterWithAPIWrapperComposed(@ProjectedValueWrapper @Wrapper x: Int) { }
}
