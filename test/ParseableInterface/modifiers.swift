// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-parseable-module-interface-path %t/Test.swiftinterface -module-name Test -disable-objc-attr-requires-foundation-module -enable-objc-interop %s
// RUN: %FileCheck %s --check-prefix FROMSOURCE --check-prefix CHECK < %t/Test.swiftinterface
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-parseable-module-interface-path - -module-name Test -enable-objc-interop | %FileCheck %s --check-prefix FROMMODULE --check-prefix CHECK

// CHECK-LABEL: final public class FinalClass {
public final class FinalClass {
  // CHECK: @inlinable final public class var a: Swift.Int {
  // FROMSOURCE-NEXT: {{^}} get {
  // FROMSOURCE-NEXT: return 3
  // FROMSOURCE-NEXT: }
  // FROMMODULE-NEXT: get{{$}}
  // CHECK-NEXT: }
  @inlinable
  public final class var a: Int {
    return 3
  }

  // CHECK: final public class var b: Swift.Int {
  // FROMSOURCE-NEXT: {{^}} @inlinable get {
  // FROMSOURCE-NEXT:   return 3
  // FROMSOURCE-NEXT: }
  // FROMMODULE-NEXT: {{^}} @inlinable get{{$}}
  // CHECK-NEXT:   set[[NEWVALUE:(\(newValue\))?]]{{$}}
  // CHECK-NEXT: }
  public final class var b: Int {
    @inlinable get {
      return 3
    }
    set {
      print("x")
    }
  }

  // CHECK: public static var c: Swift.Int {
  // CHECK-NEXT: {{^}} get
  // FROMSOURCE-NEXT:   @inlinable set[[NEWVALUE]] {}
  // FROMMODULE-NEXT:   @inlinable set[[NEWVALUE]]{{$}}
  // CHECK-NEXT: }
  public static var c: Int {
    get {
      return 0
    }
    @inlinable set {}
  }

  // CHECK: @objc dynamic final public var d: Swift.Int {
  // CHECK-NEXT: {{^}} @objc get{{$}}
  // CHECK-NEXT: {{^}} @objc set[[NEWVALUE]]{{$}}
  // CHECK-NEXT: }
  @objc public dynamic var d: Int {
    get {
      return 0
    }
    set {}
  }
}

// CHECK-LABEL: public class Base {
public class Base {
  // CHECK-NEXT: @objc public init(){{$}}
  @objc public init() {}
  // CHECK-NEXT: @objc required public init(x: Swift.Int){{$}}
  @objc public required init(x: Int) {}
  // CHECK-NEXT: @objc deinit{{$}}
} // CHECK-NEXT: {{^}$}}


// CHECK-LABEL: public class SubImplicit : {{(Test[.])?Base}} {
public class SubImplicit: Base {
  // CHECK-NEXT: @objc deinit{{$}}
  // CHECK-NEXT: @objc override public init(){{$}}
  // CHECK-NEXT: @objc required public init(x: Swift.Int){{$}}
} // CHECK-NEXT: {{^}$}}


// CHECK-LABEL: public class SubExplicit : {{(Test[.])?Base}} {
public class SubExplicit: Base {
  // Make sure adding "required" preserves both "required" and "override".
  // CHECK-NEXT: @objc override required public init(){{$}}
  public override required init() { super.init() }
  // CHECK-NEXT: @objc required public init(x: Swift.Int){{$}}
  public required init(x: Int) { super.init() }
  // CHECK-NEXT: @objc deinit{{$}}
} // CHECK-NEXT: {{^}$}}

// CHECK-LABEL: public struct MyStruct {
public struct MyStruct {
  // CHECK: public var e: Swift.Int {
  // CHECK-NEXT: {{^}} mutating get{{$}}
  // FROMSOURCE-NEXT: {{^}} @inlinable nonmutating set[[NEWVALUE]] {}
  // FROMMODULE-NEXT: {{^}} @inlinable nonmutating set[[NEWVALUE]]{{$}}
  // CHECK-NEXT: }
  public var e: Int {
    mutating get { return 0 }
    @inlinable nonmutating set {}
  }
  // CHECK-NEXT: }
}
