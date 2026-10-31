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
/// Using a class with an allocator is safe. Implementing an allocator is not:
/// the Swift runtime trusts it to return valid, suitably aligned memory that
/// stays allocated until it is passed to `deallocate`.
///
/// A subclass inherits the allocator of its superclass unless it declares its
/// own. An instance is always freed by the allocator that allocated it, even
/// when the last reference to it is released in another module or on another
/// thread, so an allocator must be safe to call from any thread.
///
/// The requirements are called from within the Swift runtime. They must not
/// allocate instances of classes that use the same allocator, and must not
/// call back into the runtime in ways that could need such an instance.
/// Allocation failure is not recoverable: an allocator that cannot allocate
/// must stop the program.
///
/// Like any heap allocation, the allocation of an instance may be optimized
/// away, or moved to the stack, if the instance does not escape; such
/// instances are not allocated by the class's allocator.
@available(SwiftStdlib 6.5, *)
@unsafe
public protocol ClassAllocator: SendableMetatype {
  /// Allocates memory for an instance of a class.
  ///
  /// - Parameters:
  ///   - byteCount: The number of bytes to allocate. This includes the
  ///     storage of tail-allocated elements, if any (see `ManagedBuffer`).
  ///   - alignment: The required alignment of the memory, in bytes. A power
  ///     of two.
  ///   - type: The class being allocated, which is the class declared with
  ///     `@allocator` or one of its subclasses.
  /// - Returns: The address of at least `byteCount` bytes of uninitialized
  ///   memory, aligned to `alignment`. Must not be null.
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer

  /// Frees memory that `allocate(byteCount:alignment:for:)` returned.
  ///
  /// - Parameters:
  ///   - pointer: The address of the memory.
  ///   - byteCount: The instance size of `type`. This is the byte count
  ///     passed to `allocate`, unless the instance had tail-allocated
  ///     elements; then it is less.
  ///   - alignment: The alignment passed to `allocate`.
  ///   - type: The class of the instance.
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  )

  /// Returns the number of usable bytes at `pointer`, or `nil` if unknown.
  ///
  /// The usable size is at least the byte count passed to `allocate`. The
  /// standard library uses it to compute `ManagedBuffer.capacity`, and stops
  /// the program if it is unknown. The default implementation returns `nil`.
  ///
  /// - Parameters:
  ///   - pointer: An address that `allocate(byteCount:alignment:for:)`
  ///     returned.
  ///   - type: The class of the instance at `pointer`.
  static func usableSize(of pointer: UnsafeRawPointer, for type: AnyClass)
    -> Int?
}

@available(SwiftStdlib 6.5, *)
extension ClassAllocator {
  public static func usableSize(
    of pointer: UnsafeRawPointer, for type: AnyClass
  ) -> Int? {
    return nil
  }
}

//===----------------------------------------------------------------------===//
// Thunks called by the runtime (runtime/ClassAllocator.cpp) with the Swift
// calling convention. After the explicit arguments, the C++ caller passes the
// allocator's metatype, then its metadata and ClassAllocator witness table as
// the generic arguments.
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.5, *)
@_silgen_name("_swift_stdlib_ClassAllocator_allocate")
internal func _ClassAllocator_allocate<A: ClassAllocator>(
  _ byteCount: Int, _ alignment: Int, _ type: AnyClass, _: A.Type
) -> UnsafeMutableRawPointer {
  return unsafe A.allocate(
    byteCount: byteCount, alignment: alignment, for: type)
}

@available(SwiftStdlib 6.5, *)
@_silgen_name("_swift_stdlib_ClassAllocator_deallocate")
internal func _ClassAllocator_deallocate<A: ClassAllocator>(
  _ pointer: UnsafeMutableRawPointer, _ byteCount: Int, _ alignment: Int,
  _ type: AnyClass, _: A.Type
) {
  unsafe A.deallocate(
    pointer, byteCount: byteCount, alignment: alignment, for: type)
}

@available(SwiftStdlib 6.5, *)
@_silgen_name("_swift_stdlib_ClassAllocator_usableSize")
internal func _ClassAllocator_usableSize<A: ClassAllocator>(
  _ pointer: UnsafeRawPointer, _ type: AnyClass, _: A.Type
) -> Int {
  return unsafe A.usableSize(of: pointer, for: type) ?? -1
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

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_objectAllocationSize")
internal func _swift_objectAllocationSize(_ object: UnsafeRawPointer) -> Int

//===----------------------------------------------------------------------===//
// Direct allocation (the DirectClassAlloc optimizer pass)
//===----------------------------------------------------------------------===//

/// Allocates an instance of `C`, a class with `@allocator(A)`, by calling `A`
/// directly. The optimizer rewrites `alloc_ref` of such a class into a call
/// of this function, then inlines it, together with `A.allocate`.
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
  return unsafe object._rawValue
}

/// Frees an instance of `C`, a class with `@allocator(A)`, from its
/// deallocating deinit by calling `A` directly. The optimizer rewrites
/// `dealloc_ref` of such a class into a call of this function. If unowned
/// references to the instance remain, the runtime frees the memory later,
/// through `A`, when the last one is released.
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
