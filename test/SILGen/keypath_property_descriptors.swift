// RUN: %target-swift-frontend -emit-silgen -enable-key-path-resilience %s | %FileCheck %s

// TODO: globals should get descriptors
public var a: Int = 0

@inlinable
public var b: Int { return 0 }

@usableFromInline
internal var c: Int = 0

// no descriptor
// CHECK-NOT: sil_property #d
internal var d: Int = 0
// CHECK-NOT: sil_property #e
private var e: Int = 0

public struct A {
  // CHECK-LABEL: sil_property #A.a
  public var a: Int = 0

  // CHECK-LABEL: sil_property #A.b
  @inlinable
  public var b: Int { return 0 }

  // CHECK-LABEL: sil_property #A.c
  @usableFromInline
  internal var c: Int = 0

  // no descriptor
  // CHECK-NOT: sil_property #A.d
  internal var d: Int = 0
  // CHECK-NOT: sil_property #A.e
  fileprivate var e: Int = 0
  // CHECK-NOT: sil_property #A.f
  private var f: Int = 0

  // TODO: static vars should get descriptors
  public static var a: Int = 0
  @inlinable
  public static var b: Int { return 0 }
  @usableFromInline
  internal static var c: Int = 0

  // no descriptor
  // CHECK-NOT: sil_property #A.d
  internal static var d: Int = 0
  // CHECK-NOT: sil_property #A.e
  fileprivate static var e: Int = 0
  // CHECK-NOT: sil_property #A.f
  private static var f: Int = 0

  // CHECK-LABEL: sil_property #A.subscript{{.*}} id @{{.*}}1a
  public subscript(a x: Int) -> Int { return x }
  // CHECK-LABEL: sil_property #A.subscript{{.*}} id @{{.*}}1b
  @inlinable
  public subscript(b x: Int) -> Int { return x }
  // CHECK-LABEL: sil_property #A.subscript{{.*}} id @{{.*}}1c
  @usableFromInline
  internal subscript(c x: Int) -> Int { return x }
  
  // no descriptor
  // CHECK-NOT: sil_property #A.subscript
  internal subscript(d x: Int) -> Int { return x }
  fileprivate subscript(e x: Int) -> Int { return x }
  private subscript(f x: Int) -> Int { return x }

  // CHECK-LABEL: sil_property #A.subscript{{.*}} id @{{.*}}1a
  public subscript<T>(a x: T) -> T { return x }
  // CHECK-LABEL: sil_property #A.subscript{{.*}} id @{{.*}}1b
  @inlinable
  public subscript<T>(b x: T) -> T { return x }
  // CHECK-LABEL: sil_property #A.subscript{{.*}} id @{{.*}}1c
  @usableFromInline
  internal subscript<T>(c x: T) -> T { return x }
  
  // no descriptor
  // CHECK-NOT: sil_property #A.subscript
  internal subscript<T>(d x: T) -> T { return x }
  fileprivate subscript<T>(e x: T) -> T { return x }
  private subscript<T>(f x: T) -> T { return x }

  // no descriptor
  public var count: Int {
    mutating get {
      _count += 1
      return _count
    }
    set {
      _count = newValue
    }
  }

  private var _count: Int = 0
}

