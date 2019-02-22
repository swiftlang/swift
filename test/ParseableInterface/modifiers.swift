// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-parseable-module-interface-path %t/Test.swiftinterface -module-name Test -disable-objc-attr-requires-foundation-module -enable-objc-interop %s
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-parseable-module-interface-path - -module-name Test -enable-objc-interop | %FileCheck %s

// CHECK-LABEL: final public class FinalClass {
public final class FinalClass {
  // CHECK: @inlinable final public class var a: [[INT:(Swift.)?Int]] {
  // CHECK-NEXT: {{^}} get {
  // CHECK-NEXT: return 3
  // CHECK-NEXT: }
  // CHECK-NEXT: }
  @inlinable
  public final class var a: Int {
    return 3
  }

  // CHECK: final public class var b: [[INT]] {
  // CHECK-NEXT:   {{^}} @inlinable get {
  // CHECK-NEXT:     return 3
  // CHECK-NEXT:   }
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

  // CHECK: public static var c: [[INT]] {
  // CHECK-NEXT: {{^}} get
  // CHECK-NEXT:   @inlinable set[[NEWVALUE]] {}
  // CHECK-NEXT: }
  public static var c: Int {
    get {
      return 0
    }
    @inlinable set {}
  }

  // CHECK: @objc dynamic final public var d: [[INT]] {
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
  // CHECK-NEXT: @objc required public init(x: [[INT]]){{$}}
  @objc public required init(x: Int) {}
  // CHECK-NEXT: @objc deinit{{$}}
} // CHECK-NEXT: {{^}$}}


// CHECK-LABEL: public class SubImplicit : {{(Test[.])?Base}} {
public class SubImplicit: Base {
  // CHECK-NEXT: @objc override public init(){{$}}
  // CHECK-NEXT: @objc required public init(x: [[INT]]){{$}}
  // CHECK-NEXT: @objc deinit{{$}}
} // CHECK-NEXT: {{^}$}}


// CHECK-LABEL: public class SubExplicit : {{(Test[.])?Base}} {
public class SubExplicit: Base {
  // Make sure adding "required" preserves both "required" and "override".
  // CHECK-NEXT: @objc override required public init(){{$}}
  public override required init() { super.init() }
  // CHECK-NEXT: @objc required public init(x: [[INT]]){{$}}
  public required init(x: Int) { super.init() }
  // CHECK-NEXT: @objc deinit{{$}}
} // CHECK-NEXT: {{^}$}}

// CHECK-LABEL: public struct MyStruct {
public struct MyStruct {
  // CHECK: public var e: [[INT]] {
  // CHECK-NEXT: {{^}} mutating get{{$}}
  // CHECK-NEXT: {{^}} @inlinable nonmutating set[[NEWVALUE]] {}
  // CHECK-NEXT: }
  public var e: Int {
    mutating get { return 0 }
    @inlinable nonmutating set {}
  }
  // CHECK-NEXT: }
}
