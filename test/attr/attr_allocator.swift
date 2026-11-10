// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple -verify-additional-prefix enabled-
// RUN: %target-typecheck-verify-swift -target %target-future-triple -verify-additional-prefix disabled-

// REQUIRES: swift_feature_ClassAllocators

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

struct NotAnAllocator {}

@allocator(Pool) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
class Base {}

// Subclasses inherit the allocator, and may declare their own, or opt out.
class Derived: Base {}
@allocator(Pool) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class Overriding: Base {}
@allocator(SystemClassAllocator) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
class OptedOut: Base {}
final class InheritsOptOut: OptedOut {}

// Generic classes are allowed, but the allocator is shared by all their
// specializations.
@allocator(Pool) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class GenericBox<T> {}

struct GenericPool<T>: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe Pool.allocate(byteCount: byteCount, alignment: alignment, for: type)
  }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe Pool.deallocate(pointer, byteCount: byteCount, alignment: alignment, for: type)
  }
}

@allocator(GenericPool<Int>) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class ConcreteGenericPool<T> {}

@allocator(GenericPool<T>) // expected-enabled-error {{allocator type 'GenericPool<T>' cannot depend on the generic parameters of 'DependentPool'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class DependentPool<T> {}

// Nested types of the class can be named.
@allocator(Nested.Heap) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class Nested {
  enum Heap: @unsafe ClassAllocator {
    static func allocate(
      byteCount: Int, alignment: Int, for type: AnyClass
    ) -> UnsafeMutableRawPointer {
      unsafe Pool.allocate(byteCount: byteCount, alignment: alignment, for: type)
    }
    static func deallocate(
      _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
      for type: AnyClass
    ) {
      unsafe Pool.deallocate(pointer, byteCount: byteCount, alignment: alignment, for: type)
    }
  }
}

// A type nested in a generic class depends on its generic parameters.
@allocator(GenericNested.Heap) // expected-enabled-error {{allocator type 'GenericNested<T>.Heap' cannot depend on the generic parameters of 'GenericNested'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class GenericNested<T> {
  enum Heap: @unsafe ClassAllocator {
    static func allocate(
      byteCount: Int, alignment: Int, for type: AnyClass
    ) -> UnsafeMutableRawPointer {
      unsafe Pool.allocate(byteCount: byteCount, alignment: alignment, for: type)
    }
    static func deallocate(
      _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
      for type: AnyClass
    ) {
      unsafe Pool.deallocate(pointer, byteCount: byteCount, alignment: alignment, for: type)
    }
  }
}

@allocator(NotAnAllocator) // expected-enabled-error {{allocator type 'NotAnAllocator' does not conform to protocol 'ClassAllocator'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class BadAllocator {}

@allocator(any ClassAllocator) // expected-enabled-error {{allocator type 'any ClassAllocator' does not conform to protocol 'ClassAllocator'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class ExistentialAllocator {}

@allocator(Missing) // expected-enabled-error {{cannot find type 'Missing' in scope}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class MissingAllocator {}

// Actors may have allocators.
@allocator(Pool) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
actor AnActor {}

// Classes that can't have an allocator.
@allocator(Pool) // expected-enabled-error {{'@allocator' cannot be applied to class 'Buffer' because it is a subclass of 'ManagedBuffer'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class Buffer: ManagedBuffer<Int, Int> {}

class BufferBase: ManagedBuffer<Int, Int> {}
@allocator(Pool) // expected-enabled-error {{'@allocator' cannot be applied to class 'DerivedBuffer' because it is a subclass of 'ManagedBuffer'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class DerivedBuffer: BufferBase {}

// ...but they may opt out.
@allocator(SystemClassAllocator) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class OptedOutBuffer: ManagedBuffer<Int, Int> {}

@allocator(Pool) // expected-enabled-error {{'@allocator' may only be used on 'class' declarations}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
struct NotAClass {}

@allocator(Pool) // expected-enabled-error {{'@allocator' may only be used on 'class' declarations}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
extension Base {}

@allocator // expected-error {{expected '(' in 'allocator' attribute}}
final class MissingParen {}

@allocator() // expected-error {{expected a type name in '@allocator()'}}
final class MissingType {}

// In a module built without library evolution, a public class's allocator
// must be visible to clients, which inherit and call it.
@allocator(Pool) // expected-enabled-error {{allocator type 'Pool' of public class 'PublicClass' must be '@usableFromInline' or public because 'Pool' is internal}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
public final class PublicClass {}

@usableFromInline
enum ExportedPool: @unsafe ClassAllocator {
  @usableFromInline
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe Pool.allocate(byteCount: byteCount, alignment: alignment, for: type)
  }
  @usableFromInline
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe Pool.deallocate(pointer, byteCount: byteCount, alignment: alignment, for: type)
  }
}

@allocator(ExportedPool) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
public final class PublicClassWithExportedPool {}

@allocator(GenericPool<Int>) // expected-enabled-error {{allocator type 'GenericPool<Int>' of open class 'OpenClass' must be '@usableFromInline' or public because 'GenericPool' is internal}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
open class OpenClass {}

// ManagedBufferPointer can't allocate a class with an allocator.
func makeBuffers(_ dynamicClass: AnyClass) {
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Base.self, minimumCapacity: 1) { _, _ in 0 } // expected-enabled-error {{'ManagedBufferPointer' cannot allocate an instance of 'Base', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Derived.self, minimumCapacity: 1) { _, _ in 0 } // expected-enabled-error {{'ManagedBufferPointer' cannot allocate an instance of 'Derived', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: OptedOut.self, minimumCapacity: 1) { _, _ in 0 }
  // Not statically known: the runtime traps.
  _ = ManagedBufferPointer<Int, Int>(bufferClass: dynamicClass, minimumCapacity: 1) { _, _ in 0 }
}
