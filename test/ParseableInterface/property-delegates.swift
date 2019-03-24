// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -module-name Test -emit-parseable-module-interface-path %t/Test.swiftinterface %s
// RUN: %FileCheck %s < %t/Test.swiftinterface --check-prefix CHECK --check-prefix NONRESILIENT
// RUN: %target-swift-frontend -build-module-from-parseable-interface %t/Test.swiftinterface -o %t/Test.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -emit-parseable-module-interface-path - %t/Test.swiftmodule -module-name Test | %FileCheck %s --check-prefix CHECK  --check-prefix NONRESILIENT

// RUN: %target-swift-frontend -typecheck -module-name TestResilient -emit-parseable-module-interface-path %t/TestResilient.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface --check-prefix CHECK --check-prefix RESILIENT

// RUN: %target-swift-frontend -build-module-from-parseable-interface %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -emit-parseable-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s --check-prefix CHECK --check-prefix RESILIENT


@propertyDelegate
public struct Wrapper<T> {
  public var value: T
}

@propertyDelegate
public struct WrapperWithInitialValue<T> {
  public var value: T

  public init(initialValue: T) {
    self.value = initialValue
  }

  public init(alternate value: T) {
    self.value = value
  }
}

// CHECK: public struct HasDelegates {
public struct HasDelegates {
  // CHECK: public var x: {{(Swift.)?}}Int {
  // CHECK-NEXT:  get
  // CHECK-NEXT:  set
  // CHECK-NEXT:}
  public var x: Int by public Wrapper

  // CHECK: public var $x: Test{{(Resilient)?}}.Wrapper<Swift.Int>

  // CHECK: public var y: Swift.Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  public private(set) var y: Int by public WrapperWithInitialValue = 17

  // CHECK-NONRESILIENT: @_hasStorage @_hasInitialValue public var $y: Test.WrapperWithInitialValue<Swift.Int> {
  // CHECK-RESILIENT: public var $y: TestResilient.WrapperWithInitialValue<Swift.Int> {
  // CHECK:  get
  // CHECK-NEXT:  }

  // CHECK: public var z: Swift.Bool {
  // CHECK-NEXT:  get
  // CHECK-NEXT:  set
  // CHECK-NEXT:}
  public var z by WrapperWithInitialValue(alternate: false)

  // CHECK-NONRESILIENT: @_hasInitialValue internal var $z: Test.WrapperWithInitialValue<Swift.Bool>
  // CHECK-RESILIENT-NOT: internal var $z
}
