// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -swift-version 5 -module-name TestResilient -emit-module-interface-path %t/TestResilient.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface --check-prefix CHECK --check-prefix RESILIENT

// RUN: %target-swift-frontend -compile-module-from-interface -swift-version 5 %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -swift-version 5  -emit-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s --check-prefix CHECK --check-prefix RESILIENT

@propertyWrapper
public struct Wrapper<T> {
  public var value: T

  public var wrappedValue: T {
    get { value }
    set { value = newValue }
  }
}

@propertyWrapper
public struct WrapperWithInitialValue<T> {
  private var value: T
  
  public var wrappedValue: T {
    get { value }
    set { value = newValue }
  }

  public init(initialValue: T) {
    self.value = initialValue
  }

  public init(alternate value: T) {
    self.value = value
  }

  public var projectedValue: Wrapper<T> {
    get { Wrapper(value: value) }
    set { value = newValue.value }
  }
}

// CHECK: public struct HasWrappers {
public struct HasWrappers {
  // CHECK: @TestResilient.Wrapper public var x: {{(Swift.)?}}Int {
  // CHECK-NEXT: get
  // CHECK-NEXT: set
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
  // CHECK-NEXT: }  
  @WrapperWithInitialValue(alternate: false) public var z
}
