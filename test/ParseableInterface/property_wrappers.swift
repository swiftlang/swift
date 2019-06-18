// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -swift-version 5 -module-name TestResilient -emit-parseable-module-interface-path %t/TestResilient.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface --check-prefix CHECK --check-prefix RESILIENT

// RUN: %target-swift-frontend -build-module-from-parseable-interface -swift-version 5 %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -swift-version 5  -emit-parseable-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s --check-prefix CHECK --check-prefix RESILIENT

@propertyWrapper
public struct Wrapper<T> {
  public var value: T
}

@propertyWrapper
public struct WrapperWithInitialValue<T> {
  public var value: T

  public init(initialValue: T) {
    self.value = initialValue
  }

  public init(alternate value: T) {
    self.value = value
  }
}

// CHECK: public struct HasWrappers {
public struct HasWrappers {
  // CHECK: @TestResilient.Wrapper public var x: {{(Swift.)?}}Int {
  // CHECK-NEXT: get
  // CHECK-NEXT: set
  // CHECK-NEXT: }  
  @Wrapper public var x: Int

  // CHECK: @TestResilient.WrapperWithInitialValue public var y: Swift.Int {
  // CHECK-NEXT: get
  // CHECK-NEXT: }  
  @WrapperWithInitialValue public private(set) var y = 17

  // CHECK: @TestResilient.WrapperWithInitialValue public var z: Swift.Bool {
  // CHECK-NEXT: get
  // CHECK-NEXT: set
  // CHECK-NEXT: }  
  @WrapperWithInitialValue(alternate: false) public var z
}
