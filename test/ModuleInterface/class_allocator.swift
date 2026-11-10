// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -enable-experimental-feature ClassAllocators -target %target-future-triple -default-class-allocator Library.InternalPool
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library -target %target-future-triple
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: swift_feature_ClassAllocators

// In a module built with library evolution, only classes that clients can't
// see may have allocators, so the interface only prints opt-outs. It doesn't
// print the module's class allocator flags, and the allocator types may be
// internal.

// CHECK-NOT: InternalPool
// CHECK-NOT: -default-class-allocator

enum InternalPool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

// CHECK-NOT: Internal
@allocator(InternalPool)
final class InternalFinal {}

class InternalDefaulted {}

// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Swift{{(::|\.)}}SystemClassAllocator) final public class Final {
@allocator(SystemClassAllocator)
public final class Final {
  public init() {}
}

// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Swift{{(::|\.)}}SystemClassAllocator) open class Open {
@allocator(SystemClassAllocator)
open class Open {
  public init() {}
}

// CHECK: final public class OpenSub : Library{{(::|\.)}}Open {
public final class OpenSub: Open {}
