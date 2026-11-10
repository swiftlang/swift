//===--- ClassAllocator.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// A type that provides the memory for instances of classes declared with
/// `@allocator`.
///
/// Declare a class with `@allocator(A)` to allocate its instances, and the
/// instances of its subclasses, with `A.allocate` and to free them with
/// `A.deallocate`, instead of with the system allocator:
///
///     enum NodePool: @unsafe ClassAllocator {
///       static func allocate(
///         byteCount: Int, alignment: Int, for type: AnyClass
///       ) -> UnsafeMutableRawPointer { ... }
///
///       static func deallocate(
///         _ pointer: UnsafeMutableRawPointer, byteCount: Int,
///         alignment: Int, for type: AnyClass
///       ) { ... }
///     }
///
///     @allocator(NodePool)
///     final class Node { ... }
///
/// Conforming to `ClassAllocator` is unsafe: the compiler trusts an allocator
/// to return valid, suitably sized and aligned memory, and to not reuse memory
/// until it has been returned. Using a class with an allocator is safe.
///
/// A subclass uses the allocator of its superclass unless it declares its
/// own. An instance is always freed by the allocator that allocated it, even
/// when its last reference is released in another module or on another
/// thread, so an allocator must be safe to call from any thread.
///
/// An allocator's requirements must not allocate instances of classes that
/// use the same allocator, and must not release the last reference to one.
///
/// Like any heap allocation, the allocation of an instance that doesn't
/// escape may be optimized away or promoted to the stack; such instances are
/// never allocated by the class's allocator.
@available(SwiftStdlib 6.5, *)
@unsafe
public protocol ClassAllocator: SendableMetatype {
  /// Returns memory for an instance of `type`.
  ///
  /// - Parameters:
  ///   - byteCount: The instance size of `type`. Always greater than zero.
  ///   - alignment: The instance alignment of `type`, a power of two.
  ///   - type: The dynamic class of the instance.
  /// - Returns: The address of at least `byteCount` uninitialized bytes,
  ///   aligned to `alignment`. Returning null is undefined behavior.
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer

  /// Takes back memory that `allocate(byteCount:alignment:for:)` returned.
  ///
  /// - Parameters:
  ///   - pointer: The address that `allocate` returned.
  ///   - byteCount: The `byteCount` that was passed to `allocate`.
  ///   - alignment: The `alignment` that was passed to `allocate`.
  ///   - type: The dynamic class of the instance.
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  )
}

/// The system allocator, as a class allocator.
///
/// `@allocator(SystemClassAllocator)` means that a class has no custom
/// allocator: it's allocated and freed like any class without `@allocator`.
/// It's allowed on every class, and opts a class out of the allocator of its
/// superclass or of `-default-class-allocator`.
@available(SwiftStdlib 6.5, *)
public enum SystemClassAllocator: @unsafe ClassAllocator {
  @inlinable
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(
      Builtin.allocRaw(byteCount._builtinWordValue,
                       alignment._builtinWordValue))
  }

  @inlinable
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    Builtin.deallocRaw(pointer._rawValue, byteCount._builtinWordValue,
                       alignment._builtinWordValue)
  }
}

//===----------------------------------------------------------------------===//
// Runtime entry points
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_initHeapObject")
internal func _swift_initHeapObject(
  _ type: AnyClass, _ allocation: UnsafeMutableRawPointer, _ size: Int,
  _ alignmentMask: Int
) -> UnsafeMutableRawPointer

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_deallocClassInstanceBegin")
internal func _swift_deallocClassInstanceBegin(
  _ object: UnsafeMutableRawPointer
) -> UnsafeMutableRawPointer?

//===----------------------------------------------------------------------===//
// Compiled allocation and deallocation
//===----------------------------------------------------------------------===//

/// Allocates an instance of `C`, a class with the class allocator `A`, by
/// calling `A` directly. The compiler rewrites every heap allocation of such
/// a class into a call of this function.
///
/// Not for direct use.
@available(SwiftStdlib 6.5, *)
@_alwaysEmitIntoClient @inline(__always)
public func _allocateClassInstance<C: AnyObject, A: ClassAllocator>(
  _: C.Type, _: A.Type, _ size: Builtin.Word, _ alignMask: Builtin.Word
) -> Builtin.RawPointer {
  let allocation = unsafe A.allocate(
    byteCount: Int(size), alignment: Int(alignMask) &+ 1, for: C.self)
  let object = unsafe _swift_initHeapObject(
    C.self, allocation, Int(size), Int(alignMask))
  return object._rawValue
}

/// Frees an instance of `C`, a class with the class allocator `A`, from its
/// deallocating deinit, by calling `A` directly. The compiler rewrites the
/// deallocation in the deallocating deinit of such a class into a call of
/// this function. If unowned references to the instance remain, the runtime
/// frees the memory through `A` when the last one is released.
///
/// Not for direct use.
@available(SwiftStdlib 6.5, *)
@_alwaysEmitIntoClient @inline(__always)
public func _deallocateClassInstance<C: AnyObject, A: ClassAllocator>(
  _: C.Type, _: A.Type, _ object: Builtin.RawPointer,
  _ size: Builtin.Word, _ alignMask: Builtin.Word
) {
  if let allocation = unsafe _swift_deallocClassInstanceBegin(
    UnsafeMutableRawPointer(object)
  ) {
    unsafe A.deallocate(
      allocation, byteCount: Int(size), alignment: Int(alignMask) &+ 1,
      for: C.self)
  }
}

//===----------------------------------------------------------------------===//
// Class allocator value witnesses
//===----------------------------------------------------------------------===//

// The value witness table of a class with a class allocator ends with a
// witness that frees an instance through its `ClassAllocator`, with the
// signature of a struct or enum's `deallocate(_:byteCount:alignment:for:)`:
// either that method itself or a function that the compiler emits for each
// allocator type it uses, which calls the allocator's witness. The compiler
// also calls `_classAllocatorAllocate`, through the same kind of function, to
// allocate a stack-promoted instance that it can't put on the stack.

/// Allocates memory from `A` for an object of the class with metadata
/// `metadata`.
///
/// Not for direct use.
@available(SwiftStdlib 6.5, *)
@_alwaysEmitIntoClient @inline(__always)
public func _classAllocatorAllocate<A: ClassAllocator>(
  _: A.Type, _ metadata: Builtin.RawPointer, _ size: Builtin.Word,
  _ alignMask: Builtin.Word
) -> Builtin.RawPointer {
  let type: AnyClass = unsafe unsafeBitCast(
    UnsafeRawPointer(metadata), to: AnyClass.self)
  let allocation = unsafe A.allocate(
    byteCount: Int(size), alignment: Int(alignMask) &+ 1, for: type)
  return allocation._rawValue
}
