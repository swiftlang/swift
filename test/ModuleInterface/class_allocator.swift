// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -enable-experimental-feature ClassAllocators -target %target-future-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library -target %target-future-triple
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: swift_feature_ClassAllocators

public enum Pool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

@usableFromInline
internal enum InternalPool: @unsafe ClassAllocator {
  @usableFromInline
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  @usableFromInline
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

// The allocator is printed fully qualified, and the declaration is guarded by
// the feature.

// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Library{{::|\.}}Pool) open class Base {
// CHECK: #endif
@allocator(Pool)
open class Base {
  public init() {}
}

// CHECK-NOT: #if compiler(>=5.3) && $ClassAllocators
// CHECK: open class Derived : Library{{::|\.}}Base {
open class Derived: Base {}

// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Library{{::|\.}}InternalPool) final public class Final {
// CHECK: #endif
@allocator(InternalPool)
public final class Final {
  public init() {}
}
