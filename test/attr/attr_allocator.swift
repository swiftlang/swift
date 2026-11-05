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

// Subclasses inherit the allocator, and may declare their own.
class Derived: Base {}
@allocator(Pool) // expected-disabled-error {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class Overriding: Base {}

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
final class ConcreteGenericPool {}

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

@allocator(NotAnAllocator) // expected-enabled-error {{allocator type 'NotAnAllocator' does not conform to protocol 'ClassAllocator'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class BadAllocator {}

@allocator(any ClassAllocator) // expected-enabled-error {{allocator type 'any ClassAllocator' does not conform to protocol 'ClassAllocator'}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class ExistentialAllocator {}

@allocator(Missing) // expected-enabled-error {{cannot find type 'Missing' in scope}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
final class MissingAllocator {}

@allocator(Pool) // expected-enabled-error {{'@allocator' cannot be applied to actor 'AnActor'; actor instances are allocated by the concurrency runtime}}
// expected-disabled-error@-1 {{'@allocator' attribute is only valid when experimental feature ClassAllocators is enabled}}
actor AnActor {}

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

// A public class's allocator must be visible to clients.
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
