// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name main -default-class-allocator main.InternalPool

// REQUIRES: swift_feature_ClassAllocators

// In a module built without library evolution, clients inherit and call the
// allocators of public classes, so they must be visible to them.

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

public class PublicClass {} // expected-error {{default class allocator 'InternalPool' of public class 'PublicClass' must be '@usableFromInline' or public because 'InternalPool' is internal}}

class InternalClass {}

@allocator(SystemClassAllocator)
public class OptedOutPublicClass {}
