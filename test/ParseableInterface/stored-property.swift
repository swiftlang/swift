// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - | %FileCheck %s

// CHECK: public struct Foo {
public struct Foo {
  // CHECK: public var computedReadOnly: [[INT:(Swift\.)?Int]] {
  // CHECK-NEXT: get
  // CHECK-NEXT: }
  public var computedReadOnly: Int { return 1 }

  // CHECK: public var computedReadWrite: [[INT]] {
  // CHECK-NEXT: get
  // CHECK-NEXT: set
  // CHECK-NEXT: }
  public var computedReadWrite: Int {
    get { return 0 }
    set { print("x") }
  }

  // CHECK: public var storedCustomSetter: [[INT]] {
  // CHECK-NEXT: didSet{{$}}
  // CHECK-NEXT: }
  public var storedCustomSetter: Int {
    didSet {}
  }

  // CHECK: public var storedTrivialSetter: [[INT]]{{$}}
  public var storedTrivialSetter: Int

  // CHECK: public private(set) var storedPrivateSet: [[INT]]{{$}}
  public private(set) var storedPrivateSet: Int

  // CHECK: public let storedImmutable: [[INT]]{{$}}
  public let storedImmutable: Int

  // FIXME: For some reason, only this VarDecl has no TypeRepr, so it always
  //        prints module qualified.
  // CHECK: @_hasInitialValue public var storedCustomSetterHasInitialValue: {{[a-zA-Z]*\.?}}Int {
  // CHECK-NEXT: willSet
  // CHECK-NEXT: didSet
  // CHECK-NEXT: }
  public var storedCustomSetterHasInitialValue: Int = 30 {
    willSet {}
    didSet {}
  }

  // CHECK: @_hasInitialValue public var storedTrivialSetterHasInitialValue: [[INT]]{{$}}
  public var storedTrivialSetterHasInitialValue: Int = 30

  // CHECK: @_hasInitialValue public var storedPrivateSetHasInitialValue: [[INT]]{{$}}
  public var storedPrivateSetHasInitialValue: Int = 30

  // CHECK: @_hasInitialValue public let storedImmutableHasInitialValue: [[INT]]{{$}}
  public let storedImmutableHasInitialValue: Int = 30
}
