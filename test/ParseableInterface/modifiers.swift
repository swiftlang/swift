// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-parseable-module-interface-path %t/Test.swiftinterface -module-name Test -disable-objc-attr-requires-foundation-module -enable-objc-interop %s
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-parseable-module-interface-path - -module-name Test -enable-objc-interop | %FileCheck %s

// CHECK: final public class FinalClass {
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

// CHECK: public struct MyStruct {
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
