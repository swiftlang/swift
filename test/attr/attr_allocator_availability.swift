// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: OS=macosx

// Class allocators need runtime support, so `@allocator` is only available
// when the deployment target, or the class's own availability, guarantees it.

@available(macOS 99.99, *)
enum Pool: @unsafe ClassAllocator {
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

@available(macOS 99.99, *)
@allocator(Pool)
final class Available {}

@available(macOS 99.99, *)
struct Outer {
  @allocator(Pool)
  final class Nested {}
}

@allocator(Pool) // expected-error {{'Pool' is only available in macOS 99.99 or newer}}
// expected-error@-1 {{'@allocator' is only available in macOS 99.99.0 or newer}}
// expected-note@+1 {{add '@available' attribute to enclosing class}}
final class Unavailable {}

@available(macOS 99.99, *)
@available(macOS, introduced: 100)
enum LaterPool: @unsafe ClassAllocator {
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

// The allocator must be available wherever the class is.
@available(macOS 99.99, *)
@allocator(LaterPool) // expected-error {{'LaterPool' is only available in macOS 100 or newer}}
final class AllocatorIsLater {}
