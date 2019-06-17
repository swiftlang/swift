// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -I %S/Inputs/implementation-only-override -DERRORS -enable-library-evolution -enable-objc-interop

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/Library.swiftinterface -I %S/Inputs/implementation-only-override -enable-library-evolution -enable-objc-interop %s
// RUN: %FileCheck %s < %t/Library.swiftinterface
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/Library.swiftinterface

// CHECK: import FooKit
// NEGATIVE-NOT: SECRET
// NEGATIVE-NOT: subscript

import FooKit
@_implementationOnly import FooKit_SECRET

// CHECK-LABEL: class GoodChild : FooKit.Parent {
//  CHECK-NEXT:   @objc override dynamic public init()
//  CHECK-NEXT:   @objc deinit
//  CHECK-NEXT: }
public class GoodChild: Parent {
  public override init() {}
  // FIXME: @_implementationOnly on an initializer doesn't exactly make sense,
  // since they're not inherited.
  @_implementationOnly public override init(SECRET: Int32) {}
  @_implementationOnly public required init(requiredSECRET: Int32) {}

  @_implementationOnly public override func methodSECRET() {} // expected-note {{overridden declaration is here}}
  @_implementationOnly public override func methodWithSECRETType() -> SECRETType? { nil } // expected-note {{overridden declaration is here}}
  @_implementationOnly public override var roPropSECRET: Parent? { nil } // expected-note {{overridden declaration is here}}
  @_implementationOnly public override var rwPropSECRET: Parent? { // expected-note {{overridden declaration is here}}
    get { nil }
    set {}
  }
  @_implementationOnly public override subscript(_ index: Int32) -> Parent? { nil } // expected-note {{overridden declaration is here}}
  @_implementationOnly public override var redefinedPropSECRET: Parent? { // expected-note {{overridden declaration is here}}
    get { nil }
    set {}
  }
}

// CHECK-LABEL: class QuietChild : FooKit.Parent {
//  CHECK-NEXT:   @objc deinit
//  CHECK-NEXT: }
public class QuietChild: Parent {
  internal override init() {}
  internal override init(SECRET: Int32) {}
  internal required init(requiredSECRET: Int32) {}
}

// CHECK-LABEL: class GoodGenericChild<Toy> : FooKit.Parent {
//  CHECK-NEXT:   @objc override dynamic public init()
//  CHECK-NEXT:   @objc deinit
//  CHECK-NEXT: }
public class GoodGenericChild<Toy>: Parent {
  public override init() {}
  // FIXME: @_implementationOnly on an initializer doesn't exactly make sense,
  // since they're not inherited.
  @_implementationOnly public override init(SECRET: Int32) {}
  @_implementationOnly public required init(requiredSECRET: Int32) {}

  @_implementationOnly public override func methodSECRET() {}
  @_implementationOnly public override func methodWithSECRETType() -> SECRETType? { nil }
  @_implementationOnly public override var roPropSECRET: Parent? { nil }
  @_implementationOnly public override var rwPropSECRET: Parent? {
    get { nil }
    set {}
  }
  @_implementationOnly public override subscript(_ index: Int32) -> Parent? { nil }
  @_implementationOnly public override var redefinedPropSECRET: Parent? {
    get { nil }
    set {}
  }
}

internal class PrivateChild: Parent {
  override func methodSECRET() {}
  override func methodWithSECRETType() -> SECRETType? { nil }
  override var roPropSECRET: Parent? { nil }
  override var rwPropSECRET: Parent? {
    get { nil }
    set {}
  }
  override subscript(_ index: Int32) -> Parent? { nil }
  override var redefinedPropSECRET: Parent? {
    get { nil }
    set {}
  }
}

internal class PrivateGrandchild: GoodChild {
  override func methodSECRET() {}
  override func methodWithSECRETType() -> SECRETType? { nil }
  override var roPropSECRET: Parent? { nil }
  override var rwPropSECRET: Parent? {
    get { nil }
    set {}
  }
  override subscript(_ index: Int32) -> Parent? { nil }
  override var redefinedPropSECRET: Parent? {
    get { nil }
    set {}
  }
}

// CHECK-LABEL: class SubscriptChild : FooKit.SubscriptParent {
//  CHECK-NEXT:   @objc deinit
//  CHECK-NEXT: }
public class SubscriptChild: SubscriptParent {
  @_implementationOnly public override subscript(_ index: Int32) -> Parent? {
    get { nil }
    set {}
  }
}

#if ERRORS

public class NaughtyChild: Parent {
  @_implementationOnly public func nonOverridingMethod() {} // expected-error {{'@_implementationOnly' can only be used on overrides}} {{3-24=}}
  @_implementationOnly public var nonOverridingProperty: Int { 0 } // expected-error {{'@_implementationOnly' can only be used on overrides}} {{3-24=}}

  public override init() {} // okay
  public override init(SECRET: Int32) {} // expected-error {{override of initializer imported as implementation-only must be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
  public required init(requiredSECRET: Int32) {} // expected-error {{override of initializer imported as implementation-only must be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}

  public override func methodSECRET() {} // expected-error {{override of instance method imported as implementation-only must be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
  public override func methodWithSECRETType() -> SECRETType? { nil } // expected-error {{override of instance method imported as implementation-only must be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }} expected-error {{cannot use class 'SECRETType' here; 'FooKit_SECRET' has been imported as implementation-only}} {{none}}
  public override var roPropSECRET: Parent? { nil } // expected-error {{override of property imported as implementation-only must be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
  public override var rwPropSECRET: Parent? { // expected-error {{override of property imported as implementation-only must be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
    get { nil }
    set {}
  }
  public override subscript(_ index: Int32) -> Parent? { nil } // expected-error {{override of subscript imported as implementation-only must be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
  public override var redefinedPropSECRET: Parent? { // FIXME: the setter here is from the implementation-only import, so we'd like to complain about this too.
    get { nil }
    set {}
  }
}

public class NaughtyChildByType: Parent {
  @_implementationOnly public override var roPropSECRET: NaughtyChildByType? { nil } // expected-error {{'@_implementationOnly' override must have the same type as the declaration it overrides ('Parent?')}} {{none}}
  @_implementationOnly public override subscript(_ index: Int32) -> NaughtyChildByType? { nil } // expected-error {{'@_implementationOnly' override must have the same type as the declaration it overrides ('(Int32) -> Parent?')}} {{none}}
}

public class NaughtyConcreteChildByType: GenericParent<Parent> {
  @_implementationOnly public override var roPropSECRET: NaughtyChildByType? { nil } // expected-error {{'@_implementationOnly' override must have the same type as the declaration it overrides ('Parent?')}} {{none}}
  @_implementationOnly public override subscript(_ index: Int32) -> NaughtyChildByType? { nil } // expected-error {{'@_implementationOnly' override must have the same type as the declaration it overrides ('(Int32) -> Parent?')}} {{none}}
}

public class NaughtyGrandchild: GoodChild {
  public override func methodSECRET() {} // expected-error {{override of '@_implementationOnly' instance method should also be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
  public override func methodWithSECRETType() -> SECRETType? { nil } // expected-error {{override of '@_implementationOnly' instance method should also be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }} expected-error {{cannot use class 'SECRETType' here; 'FooKit_SECRET' has been imported as implementation-only}} {{none}}
  public override var roPropSECRET: Parent? { nil } // expected-error {{override of '@_implementationOnly' property should also be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
  public override var rwPropSECRET: Parent? { // expected-error {{override of '@_implementationOnly' property should also be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
    get { nil }
    set {}
  }
  public override subscript(_ index: Int32) -> Parent? { nil } // expected-error {{override of '@_implementationOnly' subscript should also be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
  public override var redefinedPropSECRET: Parent? { // expected-error {{override of '@_implementationOnly' property should also be declared '@_implementationOnly'}} {{3-3=@_implementationOnly }}
    get { nil }
    set {}
  }
}

#endif // ERRORS
