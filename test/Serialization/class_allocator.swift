// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name Fragile -emit-module-path %t/Fragile.swiftmodule %s -D FRAGILE -default-class-allocator Fragile.LibPool
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name Resilient -emit-module-path %t/Resilient.swiftmodule %s -D RESILIENT -enable-library-evolution -default-class-allocator Resilient.LibPool
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -D CLIENT
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -D CLIENT -module-name main -default-class-allocator Fragile.LibPool

// REQUIRES: swift_feature_ClassAllocators

// A module records each class's allocator, including one from
// -default-class-allocator, and its opt-outs. In a module built with library
// evolution, only classes that clients can't see may have allocators, so its
// clients only see opt-outs.

#if FRAGILE || RESILIENT
public enum LibPool: @unsafe ClassAllocator {
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
#endif

#if FRAGILE
@allocator(LibPool) open class Declared {}
open class Defaulted {}
@allocator(SystemClassAllocator) open class OptedOut {}
#endif

#if RESILIENT
@allocator(SystemClassAllocator) open class OptedOut {}
final class InternalDefaulted {}
#endif

#if CLIENT
import Fragile
import Resilient

class InheritsDeclared: Fragile.Declared {}
class InheritsDefaulted: Defaulted {}
class InheritsOptOut: Fragile.OptedOut {}
class InheritsResilientOptOut: Resilient.OptedOut {}

// The compiler knows which classes have allocators.
func makeBuffers() {
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Fragile.Declared.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'Declared', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Defaulted.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'Defaulted', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: InheritsDefaulted.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'InheritsDefaulted', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Fragile.OptedOut.self, minimumCapacity: 1) { _, _ in 0 }
  _ = ManagedBufferPointer<Int, Int>(bufferClass: InheritsOptOut.self, minimumCapacity: 1) { _, _ in 0 }
  // Opt-outs win over the client's -default-class-allocator.
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Resilient.OptedOut.self, minimumCapacity: 1) { _, _ in 0 }
  _ = ManagedBufferPointer<Int, Int>(bufferClass: InheritsResilientOptOut.self, minimumCapacity: 1) { _, _ in 0 }
}
#endif
