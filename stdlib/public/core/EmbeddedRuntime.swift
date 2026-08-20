//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Class object and class metadata structures

@unsafe
public struct ClassMetadata {
  var superclassMetadata: UnsafePointer<ClassMetadata>?

  // There is no way to express the actual calling convention on this
  // function (swiftcc with 'self') currently, so let's use UnsafeRawPointer
  // and a helper function in C (_swift_embedded_invoke_heap_object_destroy).
  var destroy: UnsafeRawPointer

  // There is no way to express the actual calling convention on this
  // function (swiftcc with 'self') currently, so let's use UnsafeRawPointer
  // and a helper function in C (_swift_embedded_invoke_heap_object_optional_ivardestroyer).
  var ivarDestroyer: UnsafeRawPointer?
}

/*
  Embedded Swift Refcounting Scheme
  =================================

  The scheme for storing and maintaining a refcount on heap objects is simpler than regular Swift's. There's no side
  table, we don't track the refcount during deinit, and 16/32-bit don't support weak/unowned references. On 64-bit,
  a single count handles both weak and unowned.

  The refcount is always stored directly inline in the heap object, in the `refcount` field (see HeapObject struct
  below). On 64-bit, where weak and unowned references are supported, the field has the following structure:

  ┌──────────────┬─────────────────────┬─────────────────────────┐
  │     b63      │       b62:b32       │          b31:b0         │
  ├──────────────┼─────────────────────┼─────────────────────────┤
  │ doNotFreeBit │   weak refcount     │  number of references   │
  └──────────────┴─────────────────────┴─────────────────────────┘

  On 32-bit and 16-bit there is no weak refcount and the whole field below doNotFreeBit is the reference count:

  ┌──────────────┬──────────────────────────────────────────────┐
  │     b31      │                  b30:b0                      │
  ├──────────────┼──────────────────────────────────────────────┤
  │ doNotFreeBit │          actual number of references         │
  └──────────────┴──────────────────────────────────────────────┘

  If the highest bit (doNotFreeBit) is set, the behavior of dropping the last reference (release operation where
  refcount ends up being 0) is altered to avoid calling free() on the object (deinit is still run). This is crucial for
  class instances that are promoted by the compiler from being heap-allocated to instead be located on the stack
  (see swift_initStackObject).

  To retrieve the actual number of references from the `refcount` field, refcountMask needs to be applied, which masks
  off the doNotFreeBit and (if applicable) the weak refcount.

  When the number of references is set to all 1s i.e. immortalRefCount, the object is immortal, and retain/release on it
  do nothing. This is used for class instances that are promoted by the compiler to be allocated statically in global
  memory (see swift_initStaticObject and irgen::emitConstantObject).

  - In most cases, a class instance that is promoted to a global is still dynamically initialized with a runtime call
    to swift_initStaticObject, which writes the metadata pointer and sets the refcount field to staticRefCount.
  - As a special case to allow arrays be fully statically initialized without runtime overhead, instances of
    _ContiguousArrayStorage can be promoted to __StaticArrayStorage with the HeapObject header emitted directly by the
    compiler, refcount field included (see irgen::emitConstantObject). This also lets the object live in a read-only
    section.

  The immortalRefCount is additionally also used as a placeholder value for objects (heap-allocated or stack-allocated)
  when they're currently inside their deinit(). This is done to prevent further retains and releases inside deinit from
  triggering deinitialization again, without the need to reserve another bit for this purpose. Retains and releases in
  deinit() are allowed, as long as they are balanced at the end, i.e. the object is not escaped (user's responsibility)
  and not over-released (this can only be caused by unsafe code).

  Weak references need to distinguish between a statically allocated object and a stack object that's in the process of
  deiniting. A weak reference can be formed to a statically allocated object, but not to a deiniting stack object. Both
  have doNotFree set, and the deiniting stack object may have the immortal refcount set. We tell them apart by having
  statically allocated objects set their weak reference count to all 1s, which is a reserved value to indicate that the
  object is statically allocated.

  The following table summarizes the meaning of the possible combinations of doNotFreeBit, a saturated weak refcount,
  and having the immortal refcount value:

  ┌───────────╥──────────╥──────────╥───────────────────────────────────────────┐
  │ doNotFree ║ weak sat ║ immortal ║                                           │
  ╞═══════════╬══════════╬══════════╬═══════════════════════════════════════════╡
  │ 0         ║ no       ║ no       ║ regular class instance                    │
  ├───────────╫──────────╫──────────╫───────────────────────────────────────────┤
  │ 0         ║ no       ║ yes      ║ regular class instance during deinit()    │
  ├───────────╫──────────╫──────────╫───────────────────────────────────────────┤
  │ 0         ║ yes      ║ *        ║ impossible                                │
  ├───────────╫──────────╫──────────╫───────────────────────────────────────────┤
  │ 1         ║ no       ║ no       ║ stack-allocated, alive or maybe in deinit │
  ├───────────╫──────────╫──────────╫───────────────────────────────────────────┤
  │ 1         ║ no       ║ yes      ║ stack-allocated, definitely in deinit     │
  ├───────────╫──────────╫──────────╫───────────────────────────────────────────┤
  │ 1         ║ yes      ║ yes      ║ global-allocated                          │
  ├───────────╫──────────╫──────────╫───────────────────────────────────────────┤
  │ 1         ║ yes      ║ no       ║ impossible                                │
  └───────────╨──────────╨──────────╨───────────────────────────────────────────┘

  The last release on a stack-promoted object will set the refcount to the immortal value. However, the optimizer may
  elide the last release and directly call the deinit, in which case the refcount never gets set to immortal. Hence a
  stack object with a non-immortal refcount may or may not be in deinit. That means there's no way to reliably
  distinguish between a live stack object and a deiniting one. For our purposes, there's no need to: a weak reference to
  an object while it's still live will block stack promotion, so the only way the weak reference machinery can see one
  is if it's in deinit.


  Weak Reference Design
  =====================

  A weak reference to an object becomes logically `nil` when the object begins deinit. Loading a weak reference to a
  live object will retain that object to prevent it from being destroyed, and return the retained object. This must be
  done atomically. A concurrent release of the object must result in either an intact, retained object being returned,
  or `nil`. It must not be possible for a concurrent release to happen between a check and a retain such that the weak
  load returns a reference to an object being destroyed.

  In the Embedded runtime, weak references are implemented as pointers to the object they reference. When the object's
  strong refcount drops to zero, the object begins deinitialization. However, the object's memory is not deallocated
  until all outstanding weak references to it are gone. The outstanding weak references continue to directly point to
  this object husk, which no longer has valid storied properties, but still has a valid refcount field. When the last
  outstanding weak reference is dropped, then the object's memory is freed.

  This is implemented by having separate strong and weak reference counts. They share space within the reference count
  field. Weak references are only available on 64-bit, as the maximum counts would be too small with a smaller reference
  count field.

  Conceptually, an object holds a weak reference to itself while it's live, and that self-weak-reference is dropped
  after deinit completes. The reference counts each take action when they transition from 1 -> 0 and this framing allows
  us to consider those actions independently:

  ┌───────────────┬───────────────────┐
  │  transition   │      effect       │
  ├───────────────┼───────────────────┤
  │ strong 1 -> 0 │    begin deinit   │
  ├───────────────┼───────────────────┤
  │   weak 1 -> 0 │ deallocate object │
  └───────────────┴───────────────────┘

  This self-weak-reference is NOT actually stored in the reference count field. The field thus has an implicit +1 bias:
  a count of 0 really means 1 self-weak-reference, a count of 5 means 1 self-weak-reference plus 5 external weak
  references, etc.


  Overflow Detection
  ==================

  The Embedded runtime detects and traps when a reference count value overflows its field. However, this has
  limitations, as many reference count operations are implemented with unconditional atomic add/subtract. This means
  that the reference count manipulation is performed first, and then overflow is detected afterwards. Concurrent
  reference count manipulation can see an overflowed refcount field, and if timing is bad they may misbehave before the
  thread that hit the overflow traps.
*/
@unsafe
public struct HeapObject {
  // There is no way to express the custom ptrauth signature on the metadata
  // field, so let's use UnsafeRawPointer and a helper function in C instead
  // (_swift_embedded_set_heap_object_metadata_pointer).
  var metadata: UnsafeRawPointer?

  // The strong reference count, and on 64-bit the weak reference count.
  var refcount: Int

  // Note: The immortalRefCount value is also hard-coded in IRGen in `irgen::emitConstantObject`, and in HeapObject.h.
#if _pointerBitWidth(_64)
  static let doNotFreeBit     = Int(bitPattern: 0x8000_0000_0000_0000)
  static let weakRefcountMask = Int(bitPattern: 0x7fff_ffff_0000_0000)
  static let weakRefcountMax  = Int(bitPattern: 0x7fff_fffe_0000_0000) // The all-ones pattern is reserved for staticRefCount
  static let weakRefcountOne  = Int(bitPattern: 0x0000_0001_0000_0000)
  static let refcountMask     = Int(bitPattern: 0x0000_0000_ffff_ffff) // This MUST be at the bottom of the word
  static let immortalRefCount = Int(bitPattern: 0x0000_0000_ffff_ffff) // Make sure we don't have doNotFreeBit set
  static let staticRefCount   = Int(bitPattern: 0xffff_ffff_ffff_ffff) // Matches IRGen's swiftImmortalRefCount for global objects
#elseif _pointerBitWidth(_32)
  static let doNotFreeBit     = Int(bitPattern: 0x8000_0000)
  static let refcountMask     = Int(bitPattern: 0x7fff_ffff)
  static let immortalRefCount = Int(bitPattern: 0x7fff_ffff) // Make sure we don't have doNotFreeBit set
  static let staticRefCount   = Int(bitPattern: 0xffff_ffff) // Matches IRGen's swiftImmortalRefCount for global objects
#elseif _pointerBitWidth(_16)
  static let doNotFreeBit     = Int(bitPattern: 0x8000)
  static let refcountMask     = Int(bitPattern: 0x7fff)
  static let immortalRefCount = Int(bitPattern: 0x7fff) // Make sure we don't have doNotFreeBit set
  static let staticRefCount   = Int(bitPattern: 0xffff) // Matches IRGen's swiftImmortalRefCount for global objects
#endif

#if _pointerBitWidth(_64)
  static let immortalObjectPointerBit = UInt(0x8000_0000_0000_0000)
#endif

#if _pointerBitWidth(_64)
  static let bridgeObjectToPlainObjectMask = UInt(0x8fff_ffff_ffff_fff8)
#elseif _pointerBitWidth(_32)
  static let bridgeObjectToPlainObjectMask = UInt(0xffff_ffff)
#elseif _pointerBitWidth(_16)
  static let bridgeObjectToPlainObjectMask = UInt(0xffff)
#endif
}


/// Forward declarations of C functions

#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
// Mirrors the interface defined in swift/EmbeddedPlatform.h

@_extern(c, "_swift_allocate")
public func _swift_allocate(_ alignment: Int, _ size: Int, _ flags: CUnsignedLongLong) -> UnsafeMutableRawPointer?

@_extern(c, "_swift_deallocate")
public func _swift_deallocate(_ p: UnsafeMutableRawPointer, _ alignment: Int, _ size: Int, _ flags: CUnsignedLongLong)

@_extern(c, "_swift_generateRandom")
public func _swift_generateRandom(_ buf: UnsafeMutableRawPointer, _ nbytes: Int)

@_extern(c, "_swift_generateRandomHashSeed")
public func _swift_generateRandomHashSeed(_ buf: UnsafeMutableRawPointer, _ nbytes: Int)

@_extern(c, "_swift_typedAllocate")
public func _swift_typedAllocate(_ size: Int, _ alignMask: Int,  _ flags: CUnsignedLongLong, _ typeId: UInt64) -> UnsafeMutableRawPointer?

@_extern(c, "_swift_typedDeallocate")
public func _swift_typedDeallocate(_ buf: UnsafeMutableRawPointer, _ size: Int, _ alignMask: Int, _ flags: CUnsignedLongLong, _ typeId: UInt64)

@_extern(c, "_swift_reportError")
@usableFromInline
internal func _swift_reportError(
  _ message: UnsafePointer<UInt8>?,
  _ messageCount: Int,
  _ flags: CUnsignedLongLong
)

@_extern(c, "_swift_reportErrorAt")
@usableFromInline
internal func _swift_reportErrorAt(
  _ message: UnsafePointer<UInt8>?,
  _ messageCount: Int,
  _ fileName: UnsafePointer<UInt8>?,
  _ fileNameCount: Int,
  _ line: Int,
  _ flags: CUnsignedLongLong
)
#else
// Interface that predates the introduction of swift/EmbeddedPlatform.h

@_extern(c, "posix_memalign")
func posix_memalign(_: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _: Int, _: Int) -> CInt

@_extern(c, "free")
func free(_ p: UnsafeMutableRawPointer?)

#if os(Linux) && !SWIFT_STDLIB_HAS_ARC4RANDOM
// glibc only gained `arc4random_buf` in 2.36, and referencing it at all fails
// to link against anything older. Use `getrandom(2)` instead, which glibc has
// exposed since 2.25 and which the non-embedded Linux runtime also prefers.

@_extern(c, "getrandom")
func getrandom(
  _ buf: UnsafeMutableRawPointer, _ nbytes: Int, _ flags: CUnsignedInt
) -> Int

@_extern(c, "__errno_location")
func __errno_location() -> UnsafeMutablePointer<CInt>
#else
@_extern(c, "arc4random_buf")
func arc4random_buf(buf: UnsafeMutableRawPointer, nbytes: Int)
#endif

#endif

/// Allocations

func alignedAlloc(size: Int, alignment: Int) -> UnsafeMutableRawPointer? {
  let alignment = max(alignment, unsafe MemoryLayout<UnsafeRawPointer>.size)
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  return unsafe _swift_allocate(alignment, size, 0)
#else
  var r: UnsafeMutableRawPointer? = nil
  _ = unsafe posix_memalign(&r, alignment, size)
  return unsafe r
#endif
}

@c
public func swift_coroFrameAlloc(_ size: Int, _ type: UInt64) -> UnsafeMutableRawPointer? {
  return unsafe alignedAlloc(size: size, alignment: _swift_MinAllocationAlignment)
}

@c
public func swift_coroFrameAllocTyped(_ size: Int, _ type: UInt64) -> UnsafeMutableRawPointer? {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  return unsafe _swift_typedAllocate(size, _swift_MinAllocationAlignment - 1, 0, type)
#else
  return unsafe alignedAlloc(size: size, alignment: _swift_MinAllocationAlignment)
#endif
}

@c
public func swift_coroFrameDeallocTyped(_ ptr: UnsafeMutableRawPointer, _ type: UInt64) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  unsafe _swift_typedDeallocate(ptr, -1, _swift_MinAllocationAlignment - 1, 0, type)
#else
  unsafe free(ptr)
#endif
}

@c
public func swift_slowAlloc(_ size: Int, _ alignMask: Int) -> UnsafeMutableRawPointer? {
  let alignment: Int
  if alignMask == -1 {
    alignment = _swift_MinAllocationAlignment
  } else {
    alignment = alignMask + 1
  }
  return unsafe alignedAlloc(size: size, alignment: alignment)
}

@c
public func swift_slowDealloc(_ ptr: UnsafeMutableRawPointer, _ size: Int, _ alignMask: Int) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  unsafe _swift_deallocate(ptr, size, alignMask, 0)
#else
  unsafe free(ptr)
#endif
}

@c
public func swift_allocRawTyped(_ size: Int, _ alignMask: Int, _ typeId: UInt64) -> UnsafeMutableRawPointer? {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  return unsafe _swift_typedAllocate(size, alignMask, 0, typeId)
#else
  return unsafe swift_slowAlloc(size, alignMask)
#endif
}

@c
public func swift_deallocRawTyped(_ ptr: UnsafeMutableRawPointer, _ size: Int, _ alignMask: Int, _ typeId: UInt64) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  unsafe _swift_typedDeallocate(ptr, size, alignMask, 0, typeId)
#else
  unsafe swift_slowDealloc(ptr, size, alignMask)
#endif
}

@c
public func swift_allocObject(metadata: Builtin.RawPointer, requiredSize: Int, requiredAlignmentMask: Int) -> Builtin.RawPointer {
  return unsafe swift_allocObject(metadata: UnsafeMutablePointer<ClassMetadata>(metadata), requiredSize: requiredSize, requiredAlignmentMask: requiredAlignmentMask)._rawValue
}

func swift_allocObject(metadata: UnsafeMutablePointer<ClassMetadata>, requiredSize: Int, requiredAlignmentMask: Int) -> UnsafeMutablePointer<HeapObject> {
  let p = unsafe swift_slowAlloc(requiredSize, requiredAlignmentMask)!
  let object = unsafe p.assumingMemoryBound(to: HeapObject.self)
  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, metadata)
  unsafe object.pointee.refcount = 1
  return unsafe object
}

@c
public func swift_allocObjectTyped(metadata: Builtin.RawPointer, requiredSize: Int, requiredAlignmentMask: Int, typeId: UInt64) -> Builtin.RawPointer {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  let _p: UnsafeMutableRawPointer? = unsafe _swift_typedAllocate(requiredSize, requiredAlignmentMask, 0, typeId)
  let p = unsafe _p!
  let object = unsafe p.assumingMemoryBound(to: HeapObject.self)
  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, UnsafeMutablePointer<ClassMetadata>(metadata))
  unsafe object.pointee.refcount = 1
  return p._rawValue
#else
  swift_allocObject(metadata: metadata, requiredSize: requiredSize, requiredAlignmentMask: requiredAlignmentMask)
#endif
}

@c
public func swift_deallocUninitializedObject(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocObject(
    object: UnsafeMutablePointer<HeapObject>(object),
    allocatedSize: allocatedSize,
    allocatedAlignMask: allocatedAlignMask)
}

@c
public func swift_deallocUninitializedObjectTyped(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int, typeId: UInt64) {
  swift_deallocObjectTyped(
    object: object,
    allocatedSize: allocatedSize,
    allocatedAlignMask: allocatedAlignMask,
    typeId: typeId)
}

@c
public func swift_deallocObject(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocObject(object: UnsafeMutablePointer<HeapObject>(object), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
}

func swift_deallocObject(object: UnsafeMutablePointer<HeapObject>, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_slowDealloc(UnsafeMutableRawPointer(object), allocatedSize, allocatedAlignMask)
}

@c
public func swift_deallocObjectTyped(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int, typeId: UInt64) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  unsafe _swift_typedDeallocate(UnsafeMutableRawPointer(object), allocatedSize, allocatedAlignMask, 0, typeId)
#else
  unsafe swift_deallocObject(object: UnsafeMutablePointer<HeapObject>(object), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
#endif
}

@c
public func swift_deallocClassInstance(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocClassInstance(object: UnsafeMutablePointer<HeapObject>(object), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
}

func swift_deallocClassInstance(object: UnsafeMutablePointer<HeapObject>, allocatedSize: Int, allocatedAlignMask: Int) {
  if (unsafe object.pointee.refcount & HeapObject.doNotFreeBit) != 0 {
    return
  }

#if _pointerBitWidth(_64)
  // Release the weak refcount implicitly held by the live object on itself. If
  // there are no outstanding weak refs, this deallocates the object.
  unsafe weakRelease(object: object, allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
#else
  // Weak references aren't supported. Directly destroy the object.
  unsafe swift_slowDealloc(UnsafeMutableRawPointer(object), allocatedSize, allocatedAlignMask)
#endif
}

@c
public func swift_deallocClassInstanceTyped(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int, typeId: UInt64) {
  let p = unsafe UnsafeMutablePointer<HeapObject>(object)
  if (unsafe p.pointee.refcount & HeapObject.doNotFreeBit) != 0 {
    return
  }

#if _pointerBitWidth(_64)
  // Release the weak refcount implicitly held by the live object on itself. If
  // there are no outstanding weak refs, this deallocates the object.
  unsafe weakRelease(object: p, allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask, typeId: typeId)
#elseif SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  unsafe _swift_typedDeallocate(UnsafeMutableRawPointer(p), allocatedSize, allocatedAlignMask, 0, typeId)
#else
  unsafe swift_deallocClassInstance(object: p, allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
#endif
}

@c
public func swift_deallocPartialClassInstance(object: Builtin.RawPointer, metadata: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocPartialClassInstance(object: UnsafeMutablePointer<HeapObject>(object), metadata: UnsafeMutablePointer<ClassMetadata>(metadata), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
}

func swift_deallocPartialClassInstance(object: UnsafeMutablePointer<HeapObject>, metadata: UnsafePointer<ClassMetadata>, allocatedSize: Int, allocatedAlignMask: Int) {
  var classMetadata = unsafe _swift_embedded_get_heap_object_metadata_pointer(object).assumingMemoryBound(to: ClassMetadata.self)
  while unsafe classMetadata != metadata {
    unsafe _swift_embedded_invoke_heap_object_optional_ivardestroyer(object, classMetadata)
    guard let superclassMetadata = unsafe classMetadata.pointee.superclassMetadata else { break }
    unsafe classMetadata = superclassMetadata
  }
}

@c
public func swift_deallocPartialClassInstanceTyped(object: Builtin.RawPointer, metadata: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int, typeId: UInt64) {
  unsafe swift_deallocPartialClassInstance(object: UnsafeMutablePointer<HeapObject>(object), metadata: UnsafeMutablePointer<ClassMetadata>(metadata), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
}

@c
public func swift_initStaticObject(metadata: Builtin.RawPointer, object: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_initStaticObject(metadata: UnsafeMutablePointer<ClassMetadata>(metadata), object: UnsafeMutablePointer<HeapObject>(object))._rawValue
}

func swift_initStaticObject(metadata: UnsafeMutablePointer<ClassMetadata>, object: UnsafeMutablePointer<HeapObject>) -> UnsafeMutablePointer<HeapObject> {
  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, metadata)
  unsafe object.pointee.refcount = HeapObject.staticRefCount
  return unsafe object
}

@c
public func swift_initStackObject(metadata: Builtin.RawPointer, object: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_initStackObject(metadata: UnsafeMutablePointer<ClassMetadata>(metadata), object: UnsafeMutablePointer<HeapObject>(object))._rawValue
}

func swift_initStackObject(metadata: UnsafeMutablePointer<ClassMetadata>, object: UnsafeMutablePointer<HeapObject>) -> UnsafeMutablePointer<HeapObject> {
  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, metadata)
  unsafe object.pointee.refcount = 1 | HeapObject.doNotFreeBit
  return unsafe object
}

@unsafe
public var _emptyBoxStorage: (Int, Int) = (/*isa*/0, /*refcount*/HeapObject.staticRefCount)

@c
public func swift_allocEmptyBox() -> Builtin.RawPointer {
  let box = unsafe Builtin.addressof(&_emptyBoxStorage)
  swift_retain(object: box)
  return box
}

/// The embedded swift_allocBox version is different to the standad one in that
/// we want to avoid building metadata for the box type. Instead we store the
/// metadata of the contained type in the heap object. To make this work when
/// destroying the box the release needs to be special i.e `swift_releaseBox`.
/// It does not call the the heap object metadata's destroy function. Rather, it
/// knows that the allocBox's metadata is the contained objects and calls an
/// appropriate implementation: `_swift_embedded_invoke_box_destroy`.
/// Therefore, one cannot not use `swift_release` but rather must use
/// `swift_releaseBox` to implement the "release" function of a box object.

/// Computes the footprint of a box whose payload is described by `metadata`:
/// the offset at which the payload starts within the allocation, the total
/// allocation size, and the alignment mask to request from the allocator.
/// `swift_allocBox` and `swift_deallocBox` must agree on these values.
func _boxAllocationLayout(
  metadata: UnsafeRawPointer
) -> (startOfBoxedValue: Int, size: Int, alignMask: Int) {
  let payloadAlignMask = Int(unsafe _swift_embedded_metadata_get_align_mask(metadata))
  let payloadSize = Int(unsafe _swift_embedded_metadata_get_size(metadata))
  let headerSize = unsafe MemoryLayout<Int>.size + MemoryLayout<UnsafeRawPointer>.size
  let headerAlignMask = unsafe MemoryLayout<UnsafeRawPointer>.alignment - 1
  let startOfBoxedValue = (headerSize + payloadAlignMask) & ~payloadAlignMask
  return (
    startOfBoxedValue: startOfBoxedValue,
    size: startOfBoxedValue + payloadSize,
    alignMask: payloadAlignMask | headerAlignMask
  )
}

@_silgen_name("swift_allocBox")
public func swift_allocBox(_ metadata: Builtin.RawPointer) -> (Builtin.RawPointer, Builtin.RawPointer) {
  let layout = unsafe _boxAllocationLayout(metadata: UnsafeMutableRawPointer(metadata))

  let p = unsafe swift_slowAlloc(layout.size, layout.alignMask)!
  let object = unsafe p.assumingMemoryBound(to: HeapObject.self)

  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, UnsafeMutableRawPointer(metadata))
  unsafe object.pointee.refcount = 1

  let boxedValueAddr = unsafe UnsafeMutableRawPointer(p).advanced(by: layout.startOfBoxedValue)

  return (object._rawValue, boxedValueAddr._rawValue)
}

@_silgen_name("swift_allocBoxTyped")
public func swift_allocBoxTyped(_ metadata: Builtin.RawPointer, _ typeId: UInt64) -> (Builtin.RawPointer, Builtin.RawPointer) {
  let layout = unsafe _boxAllocationLayout(metadata: UnsafeMutableRawPointer(metadata))

#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  let p = unsafe _swift_typedAllocate(layout.size, layout.alignMask, 0, typeId)!
#else
  let p = unsafe swift_slowAlloc(layout.size, layout.alignMask)!
#endif
  let object = unsafe p.assumingMemoryBound(to: HeapObject.self)

  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, UnsafeMutableRawPointer(metadata))
  unsafe object.pointee.refcount = 1

  let boxedValueAddr = unsafe UnsafeMutableRawPointer(p).advanced(by: layout.startOfBoxedValue)

  return (object._rawValue, boxedValueAddr._rawValue)
}

@c
public func swift_deallocBox(_ pointer: UnsafeMutableRawPointer) {
  let object = unsafe pointer.bindMemory(to: HeapObject.self, capacity: 1)
  let metadata = unsafe _swift_embedded_get_heap_object_metadata_pointer(object)
  let layout = unsafe _boxAllocationLayout(metadata: metadata)
  unsafe swift_slowDealloc(
    UnsafeMutableRawPointer(object),
    layout.size,
    layout.alignMask
  )
}

/// Extracts the type metadata out of a heap object.
@c
public func swift_getObjectType(_ pointer: UnsafeMutableRawPointer) -> UnsafeRawPointer {
  let object = unsafe pointer.bindMemory(to: HeapObject.self, capacity: 1)
  return unsafe _swift_embedded_get_heap_object_metadata_pointer(object)
}

/// Determines whether two witness tables are the same.
///
/// The only way this can be true in Embedded Swift is if they refer to the same
/// address in memory.
@c
public func swift_compareWitnessTables(_ lhs: UnsafeRawPointer, _ rhs: UnsafeRawPointer) -> Bool {
  unsafe lhs == rhs
}

// MARK: - Error boxing (swift_allocError / swift_deallocError / swift_getErrorValue)

/// Error box layout: [HeapObject header] [type ptr] [errorConformance ptr] [alignment padding] [value]

/// Compute the byte offset from the start of an error box to the stored value,
/// and the total allocation size needed for the box.
@usableFromInline
internal func _errorBoxLayout(
  metadata: UnsafeRawPointer
) -> (startOfValue: Int, totalSize: Int, totalAlignMask: Int) {
  let alignMask = Int(unsafe _swift_embedded_metadata_get_align_mask(
    UnsafeMutableRawPointer(mutating: metadata)))
  let size = Int(unsafe _swift_embedded_metadata_get_size(
    UnsafeMutableRawPointer(mutating: metadata)))
  let headerSize = unsafe MemoryLayout<HeapObject>.size
    + 2 * MemoryLayout<UnsafeRawPointer>.size
  let headerAlignMask = unsafe MemoryLayout<UnsafeRawPointer>.alignment - 1
  let startOfValue = (headerSize + alignMask) & ~alignMask
  return (startOfValue, startOfValue + size, alignMask | headerAlignMask)
}

/// Read the type metadata, error conformance, and value address from an error box.
@usableFromInline
internal func _errorBoxContents(
  _ p: UnsafeRawPointer
) -> (type: UnsafeRawPointer, conformance: UnsafeRawPointer, value: UnsafeRawPointer) {
  let type = unsafe (p + MemoryLayout<HeapObject>.size)
    .assumingMemoryBound(to: UnsafeRawPointer.self).pointee
  let conformance = unsafe (p + MemoryLayout<HeapObject>.size + MemoryLayout<UnsafeRawPointer>.size)
    .assumingMemoryBound(to: UnsafeRawPointer.self).pointee
  let layout = unsafe _errorBoxLayout(metadata: type)
  let value = unsafe p.advanced(by: layout.startOfValue)
  return unsafe (type, conformance, value)
}

/// Error box destroy implementation. Called from the C calling convention bridge
/// `_swift_embedded_error_box_destroy` (in EmbeddedShims.h), which receives the object
/// via swiftself (as required by HeapObjectDestroyer) and forwards it here as a regular
/// swiftcc parameter. This indirection is necessary because Swift cannot produce a
/// function with the swiftself attribute on a free function parameter.
/// Linked transitively via the Swift reference in `_ensureErrorMetadataInitialized`.
@_silgen_name("_swift_embedded_error_destroy_impl") @export(implementation)
public func _errorBoxDestroyImpl(
  _ object: Builtin.RawPointer
) {
  let p = UnsafeMutableRawPointer(object)
  let contents = unsafe _errorBoxContents(UnsafeRawPointer(p))
  unsafe _swift_embedded_metadata_destroy(
    UnsafeMutableRawPointer(mutating: contents.type),
    UnsafeMutableRawPointer(mutating: contents.value))
  let layout = unsafe _errorBoxLayout(metadata: contents.type)
  Builtin.deallocErrorBoxTyped(
    object, layout.totalSize._builtinWordValue, layout.totalAlignMask._builtinWordValue)
}

/// Metadata storage for error boxes. Layout matches ClassMetadata: [superclass, destroy, ivarDestroyer].
/// Uses a tuple instead of ClassMetadata because @_silgen_name requires a compile-time constant
/// initializer, and struct initializers (even with all-nil fields) are not compile-time constants.
/// linkonce_odr linkage ensures a single copy after linking (pointer identity).
@_silgen_name("_swift_embedded_error_metadata_storage")
var _errorMetadataStorage:
  (superclass: UnsafeRawPointer?, destroy: UnsafeRawPointer?, ivarDestroyer: UnsafeRawPointer?)
  = (superclass: nil, destroy: nil, ivarDestroyer: nil)

private var _errorMetadataInitialized = false
// Holds a Swift reference to _errorBoxDestroyImpl so the SIL linker includes it
// transitively when swift_allocError is linked. Without this, the impl is only
// referenced from C (the swiftself wrapper in EmbeddedShims.h) which the SIL
// linker can't follow. The reference is written once during metadata init.
private var _errorBoxDestroyImplRef: (Builtin.RawPointer) -> Void = { _ in }

private func _ensureErrorMetadataInitialized() {
  guard !_errorMetadataInitialized else { return }
  _errorBoxDestroyImplRef = _errorBoxDestroyImpl
  let destroyPtr = unsafe _swift_embedded_error_destroy_ptr()
  unsafe withUnsafeMutablePointer(to: &_errorMetadataStorage.destroy) { p in
    unsafe (p.pointee = UnsafeRawPointer(destroyPtr))
  }
  _errorMetadataInitialized = true
}

/// Allocate a heap box for an `any Error` existential.
@_silgen_name("swift_allocError")
public func swift_allocError(
  _ metadata: Builtin.RawPointer,          // concrete error type metadata
  _ errorConformance: Builtin.RawPointer,  // Error witness table
  _ initialValue: Builtin.RawPointer,      // initial value (null = none)
  _ isTake: Bool                           // true = take, false = copy
) -> (Builtin.RawPointer, Builtin.RawPointer) {
  let layout = unsafe _errorBoxLayout(metadata: UnsafeRawPointer(metadata))

  _ensureErrorMetadataInitialized()
  let metaPtr = unsafe Builtin.addressof(&_errorMetadataStorage)
  let objectPtr = Builtin.allocErrorBoxTyped(
    metaPtr,
    layout.totalSize._builtinWordValue,
    layout.totalAlignMask._builtinWordValue)
  let p = UnsafeMutableRawPointer(objectPtr)

  // Store type and errorConformance after the HeapObject header
  unsafe (p + MemoryLayout<HeapObject>.size)
    .assumingMemoryBound(to: UnsafeRawPointer.self)
    .pointee = UnsafeRawPointer(metadata)
  unsafe (p + MemoryLayout<HeapObject>.size
    + MemoryLayout<UnsafeRawPointer>.size)
    .assumingMemoryBound(to: UnsafeRawPointer.self)
    .pointee = UnsafeRawPointer(errorConformance)

  let valueAddr = unsafe p.advanced(by: layout.startOfValue)

  if let src = unsafe UnsafeMutableRawPointer(bitPattern: Int(Builtin.ptrtoint_Word(initialValue))) {
    if isTake {
      unsafe _swift_embedded_metadata_initialize_with_take(
        UnsafeMutableRawPointer(metadata), valueAddr, src)
    } else {
      unsafe _swift_embedded_metadata_initialize_with_copy(
        UnsafeMutableRawPointer(metadata), valueAddr, src)
    }
  }

  return (objectPtr, valueAddr._rawValue)
}

/// Deallocate an error box whose value has already been destroyed (error-path cleanup).
@c
public func swift_deallocError(
  _ box: Builtin.RawPointer,
  _ metadata: Builtin.RawPointer
) {
  let layout = unsafe _errorBoxLayout(metadata: UnsafeRawPointer(metadata))
  Builtin.deallocErrorBoxTyped(
    box, layout.totalSize._builtinWordValue, layout.totalAlignMask._builtinWordValue)
}

/// Extract the value address, type metadata, and error conformance from an error box.
/// Writes a (OpaqueValue*, TypeMetadata*, WitnessTable*) triple to `out`.
@c
public func swift_getErrorValue(
  _ box: Builtin.RawPointer,
  _ scratch: Builtin.RawPointer,
  _ out: Builtin.RawPointer
) {
  let p = UnsafeRawPointer(box)
  let contents = unsafe _errorBoxContents(p)

  // Write (value*, type*, witness*) to the output triple
  let outPtr = unsafe UnsafeMutableRawPointer(out)
    .assumingMemoryBound(
      to: (UnsafeMutableRawPointer, UnsafeRawPointer, UnsafeRawPointer).self)
  unsafe outPtr.pointee = (
    UnsafeMutableRawPointer(mutating: contents.value),
    contents.type,
    contents.conformance
  )
}

/// Error-specific retain (same as swift_retain; no ObjC bridging in embedded).
@c
public func swift_errorRetain(_ object: Builtin.RawPointer) -> Builtin.RawPointer {
  swift_retain(object: object)
  return object
}

/// Error-specific release (same as swift_release; no ObjC bridging in embedded).
@c
public func swift_errorRelease(_ object: Builtin.RawPointer) {
  swift_release(object: object)
}

@_silgen_name("swift_makeBoxUnique")
public func swifft_makeBoxUnique(buffer: Builtin.RawPointer, metadata: Builtin.RawPointer, alignMask: Int) -> (Builtin.RawPointer, Builtin.RawPointer){
  let addrOfHeapObjectPtr = unsafe UnsafeMutablePointer<Builtin.RawPointer>(buffer)
  let box = unsafe addrOfHeapObjectPtr.pointee
  let headerSize = unsafe MemoryLayout<Int>.size + MemoryLayout<UnsafeRawPointer>.size
  let startOfBoxedValue = ((headerSize + alignMask) & ~alignMask)
  let oldObjectAddr = unsafe UnsafeMutableRawPointer(box) + startOfBoxedValue

  if !swift_isUniquelyReferenced_native(object: box) {
    let refAndObjectAddr = swift_allocBox(metadata)
    unsafe _swift_embedded_initialize_box(UnsafeMutableRawPointer(metadata), UnsafeMutableRawPointer(refAndObjectAddr.1), oldObjectAddr)
    unsafe swift_releaseBox(UnsafeMutableRawPointer(box))
    unsafe addrOfHeapObjectPtr.pointee = refAndObjectAddr.0
    return refAndObjectAddr
  } else {
    return (box, oldObjectAddr._rawValue)
  }
}

@_silgen_name("swift_makeBoxUniqueTyped")
public func swift_makeBoxUniqueTyped(buffer: Builtin.RawPointer, metadata: Builtin.RawPointer, alignMask: Int, typeId: UInt64) -> (Builtin.RawPointer, Builtin.RawPointer){
  let addrOfHeapObjectPtr = unsafe UnsafeMutablePointer<Builtin.RawPointer>(buffer)
  let box = unsafe addrOfHeapObjectPtr.pointee
  let headerSize = unsafe MemoryLayout<Int>.size + MemoryLayout<UnsafeRawPointer>.size
  let startOfBoxedValue = ((headerSize + alignMask) & ~alignMask)
  let oldObjectAddr = unsafe UnsafeMutableRawPointer(box) + startOfBoxedValue

  if !swift_isUniquelyReferenced_native(object: box) {
    let refAndObjectAddr = swift_allocBoxTyped(metadata, typeId)
    unsafe _swift_embedded_initialize_box(UnsafeMutableRawPointer(metadata), UnsafeMutableRawPointer(refAndObjectAddr.1), oldObjectAddr)
    unsafe swift_releaseBoxTyped(UnsafeMutableRawPointer(box), typeId)
    unsafe addrOfHeapObjectPtr.pointee = refAndObjectAddr.0
    return refAndObjectAddr
  } else {
    return (box, oldObjectAddr._rawValue)
  }
}

/// Refcounting

func isValidPointerForNativeRetain(object: Builtin.RawPointer) -> Bool {
  let objectBits = UInt(Builtin.ptrtoint_Word(object))
  if objectBits == 0 { return false }

  #if _pointerBitWidth(_64)
  if (objectBits & HeapObject.immortalObjectPointerBit) != 0 { return false }
  #endif

  return true
}

@c
public func swift_setDeallocating(object: Builtin.RawPointer) {
}

@c
public func swift_isEscapingClosureAtFileLocation(object: Builtin.RawPointer, filename: UnsafePointer<CChar>, filenameLength: Int32, line: Int32, column: Int32, verificationType: CUnsignedInt) -> Bool {
  let objectBits = UInt(Builtin.ptrtoint_Word(object))
  if objectBits == 0 { return false }

  guard swift_isUniquelyReferenced_native(object: object) else {
    fatalError("non-escaping closure escaped")
  }
  return false
}

@c
public func swift_isUniquelyReferenced_native(object: Builtin.RawPointer) -> Bool {
  if !isValidPointerForNativeRetain(object: object) { return false }

  return unsafe swift_isUniquelyReferenced_nonNull_native(object: UnsafeMutablePointer<HeapObject>(object))
}

@c
public func swift_isUniquelyReferenced_nonNull_native(object: Builtin.RawPointer) -> Bool {
  return unsafe swift_isUniquelyReferenced_nonNull_native(object: UnsafeMutablePointer<HeapObject>(object))
}

func swift_isUniquelyReferenced_nonNull_native(object: UnsafeMutablePointer<HeapObject>) -> Bool {
  let refcount = unsafe refcountPointer(for: object)
  return unsafe loadAcquire(refcount) & HeapObject.refcountMask == 1
}

@c
@discardableResult
public func swift_retain(object: Builtin.RawPointer) -> Builtin.RawPointer {
  if !isValidPointerForNativeRetain(object: object) { return object }

  let o = unsafe UnsafeMutablePointer<HeapObject>(object)
  return unsafe swift_retain_n_(object: o, n: 1)._rawValue
}

// Cannot use UnsafeMutablePointer<HeapObject>? directly in the function argument or return value as it causes IRGen crashes
@c
public func swift_retain_n(object: Builtin.RawPointer, n: UInt32) -> Builtin.RawPointer {
  if !isValidPointerForNativeRetain(object: object) { return object }

  let o = unsafe UnsafeMutablePointer<HeapObject>(object)
  return unsafe swift_retain_n_(object: o, n: n)._rawValue
}

func swift_retain_n_(object: UnsafeMutablePointer<HeapObject>, n: UInt32) -> UnsafeMutablePointer<HeapObject> {
  let refcount = unsafe refcountPointer(for: object)
  if unsafe loadRelaxed(refcount) & HeapObject.refcountMask == HeapObject.immortalRefCount {
    return unsafe object
  }

  let oldValue = unsafe addRelaxed(refcount, n: Int(n))

  if (oldValue & HeapObject.refcountMask) >= HeapObject.immortalRefCount - Int(n) {
    fatalError("reference count overflow")
  }

  return unsafe object
}

#if _pointerBitWidth(_64)
// Retain `object` unless its refcount holds the immortal value, and return the
// refcount value that the decision was made on. The caller passes that value to
// refcountValueIsLiveForWeakReference to find out whether the object was live,
// and therefore whether it now holds a strong reference.
//
// This is almost the same operation as swift_retain_n_(1), but it avoids a race
// between checking for immortalRefCount and doing the increment.
func tryRetain(object: UnsafeMutablePointer<HeapObject>) -> Int {
  let refcount = unsafe refcountPointer(for: object)
  var refcountValue = unsafe loadRelaxed(refcount)

  // If we see immortalRefCount then there's nothing to do, the retain operation
  // is a no-op.
  while refcountValue & HeapObject.refcountMask != HeapObject.immortalRefCount {
    // Compare-and-swap the incremented value. Use &+ to avoid an overflow
    // check. Overflow is impossible by construction since it would require
    // refcount == immortalRefCount, but the compiler doesn't realize this.
    let newValue = refcountValue &+ 1
    let (seenValue, won) = unsafe compareExchangeRelaxed(refcount, expectedOldValue: refcountValue, desiredNewValue: newValue)
    if won {
      return refcountValue
    }

    // Compare-and-swap operation failed, try again.
    refcountValue = seenValue
  }

  return refcountValue
}
#endif

@c
@discardableResult
public func swift_bridgeObjectRetain(object: Builtin.RawPointer) -> Builtin.RawPointer {
  return swift_bridgeObjectRetain_n(object: object, n: 1)
}

@c
public func swift_bridgeObjectRetain_n(object: Builtin.RawPointer, n: UInt32) -> Builtin.RawPointer {
  let objectBits = UInt(Builtin.ptrtoint_Word(object))
  let untaggedObject = Builtin.inttoptr_Word((objectBits & HeapObject.bridgeObjectToPlainObjectMask)._builtinWordValue)
  _ = swift_retain_n(object: untaggedObject, n: n)
  return object
}

@c
public func swift_release(object: Builtin.RawPointer) {
  if !isValidPointerForNativeRetain(object: object) { return }

  let o = unsafe UnsafeMutablePointer<HeapObject>(object)
  unsafe swift_release_n_(object: o, n: 1)
}

@c
public func swift_release_n(object: Builtin.RawPointer, n: UInt32) {
  if !isValidPointerForNativeRetain(object: object) { return }

  let o = unsafe UnsafeMutablePointer<HeapObject>(object)
  unsafe swift_release_n_(object: o, n: n)
}

// Non-atomic refcount entry points. For now, just route to the atomic versions.
// This can be optimized later.
@c @discardableResult
public func swift_nonatomic_retain(object: Builtin.RawPointer) -> Builtin.RawPointer {
  return swift_retain(object: object)
}

@c @discardableResult
public func swift_nonatomic_retain_n(object: Builtin.RawPointer, n: UInt32) -> Builtin.RawPointer {
  return swift_retain_n(object: object, n: n)
}

@c
public func swift_nonatomic_release(object: Builtin.RawPointer) {
  swift_release(object: object)
}

@c
public func swift_nonatomic_release_n(object: Builtin.RawPointer, n: UInt32) {
  swift_release_n(object: object, n: n)
}

func swift_release_n_(object: UnsafeMutablePointer<HeapObject>?, n: UInt32, isBoxRelease: Bool = false, typeId: UInt64 = 0) {
  guard let object = unsafe object else {
    return
  }

  let refcount = unsafe refcountPointer(for: object)
  let loadedRefcount = unsafe loadRelaxed(refcount)
  if loadedRefcount & HeapObject.refcountMask == HeapObject.immortalRefCount {
    return
  }

  let resultingRefcountValue = unsafe subFetchAcquireRelease(refcount, n: Int(n))
  if resultingRefcountValue & HeapObject.refcountMask == 0 {
    // Set the refcount to immortalRefCount before calling the object destroyer
    // to prevent future retains/releases from having any effect. Unlike the
    // full Swift runtime, we don't track the refcount inside deinit, so we
    // won't be able to detect escapes or over-releases of `self` in deinit. We
    // might want to reconsider that in the future.

    let doNotFree = (loadedRefcount & HeapObject.doNotFreeBit) != 0
    let deallocatingRefcountAndFlag = HeapObject.immortalRefCount | (doNotFree ? HeapObject.doNotFreeBit : 0)

#if _pointerBitWidth(_64)
    // When weak references are supported, we have to check the weak refcount
    // and handle things differently when there are still outstanding weak refs.
    if (resultingRefcountValue & HeapObject.weakRefcountMask) == 0 {
      // There can only be one thread with a reference at this point because
      // we're releasing the last strong reference and there are no weak
      // references, so a relaxed store is enough.
      unsafe storeRelaxed(refcount, newValue: deallocatingRefcountAndFlag)
    } else {
      // There are one or more weak references to this object, which may
      // concurrently take us from strong refcount 0 -> 1. Do a compare and swap
      // to ensure we only transition to deallocating if nobody else incremented
      // our strong refcount.
      var oldValue = resultingRefcountValue
      var done = false
      while !done {
        if (oldValue & HeapObject.refcountMask) != 0 {
          // Something retained this object before we could move to the
          // deallocating state, so we're no longer doing that here. We already
          // did the refcount decrement, so we're all done.
          return
        }

        // Still at (or retained but came back to) refcount 0, try to emplace
        // the deallocating state.
        let newValue = (oldValue & HeapObject.weakRefcountMask) | deallocatingRefcountAndFlag
        (oldValue, done) = unsafe compareExchangeRelaxed(refcount, expectedOldValue: oldValue, desiredNewValue: newValue)
      }
    }
#else
    // There can only be one thread with a reference at this point because we're
    // releasing the last existing reference and weak references aren't
    // supported, so a relaxed store is enough.
    unsafe storeRelaxed(refcount, newValue: deallocatingRefcountAndFlag)
#endif

    if isBoxRelease {
        // _swift_embedded_invoke_box_destroy only runs the boxed payload's
        // destroy witness and doesn't deallocate the box (a memory leak
        // otherwise). We deallocate it here.
        unsafe _swift_embedded_invoke_box_destroy(object)

        let metadata = unsafe _swift_embedded_get_heap_object_metadata_pointer(object)
        let layout = unsafe _boxAllocationLayout(metadata: metadata)
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
        if typeId != 0 {
          unsafe _swift_typedDeallocate(UnsafeMutableRawPointer(object), layout.size, layout.alignMask, 0, typeId)
        } else {
          unsafe _swift_deallocate(UnsafeMutableRawPointer(object), layout.size, layout.alignMask, 0)
        }
#else
        unsafe swift_slowDealloc(UnsafeMutableRawPointer(object), layout.size, layout.alignMask)
#endif
    } else {
        unsafe _swift_embedded_invoke_heap_object_destroy(object)
    }
  }
}

@c
public func swift_releaseBox(_ box: UnsafeMutableRawPointer) {
  let object = box._rawValue
  if !isValidPointerForNativeRetain(object: object) {
    fatalError("not a valid pointer for releaseBox")
  }
  let o = unsafe UnsafeMutablePointer<HeapObject>(object)
  unsafe swift_release_n_(object: o, n: 1, isBoxRelease: true)
}

@c
public func swift_releaseBoxTyped(_ box: UnsafeMutableRawPointer, _ typeId: UInt64) {
  let object = box._rawValue
  if !isValidPointerForNativeRetain(object: object) {
    fatalError("not a valid pointer for releaseBox")
  }
  let o = unsafe UnsafeMutablePointer<HeapObject>(object)
  unsafe swift_release_n_(object: o, n: 1, isBoxRelease: true, typeId: typeId)
}

@c
public func swift_bridgeObjectRelease(object: Builtin.RawPointer) {
  swift_bridgeObjectRelease_n(object: object, n: 1)
}

@c
public func swift_bridgeObjectRelease_n(object: Builtin.RawPointer, n: UInt32) {
  let objectBits = UInt(Builtin.ptrtoint_Word(object))
  let untaggedObject = Builtin.inttoptr_Word((objectBits & HeapObject.bridgeObjectToPlainObjectMask)._builtinWordValue)
  swift_release_n(object: untaggedObject, n: n)
}

@c
public func swift_retainCount(object: Builtin.RawPointer) -> Int {
  if !isValidPointerForNativeRetain(object: object) { return 0 }
  let o = unsafe UnsafeMutablePointer<HeapObject>(object)
  let refcount = unsafe refcountPointer(for: o)
  return unsafe loadAcquire(refcount) & HeapObject.refcountMask
}

/// Refcount helpers

fileprivate func refcountPointer(for object: UnsafeMutablePointer<HeapObject>) -> UnsafeMutablePointer<Int> {
  // TODO: This should use MemoryLayout<HeapObject>.offset(to: \.refcount) but we don't have KeyPaths yet
  return unsafe UnsafeMutablePointer<Int>(UnsafeRawPointer(object).advanced(by: MemoryLayout<Int>.size)._rawValue)
}

fileprivate func loadRelaxed(_ atomic: UnsafeMutablePointer<Int>) -> Int {
  Int(Builtin.atomicload_monotonic_Word(atomic._rawValue))
}

fileprivate func loadAcquire(_ atomic: UnsafeMutablePointer<Int>) -> Int {
  Int(Builtin.atomicload_acquire_Word(atomic._rawValue))
}

fileprivate func subFetchAcquireRelease(_ atomic: UnsafeMutablePointer<Int>, n: Int) -> Int {
  let oldValue = Int(Builtin.atomicrmw_sub_acqrel_Word(atomic._rawValue, n._builtinWordValue))
  // The atomicrmw operation wraps on overflow, so do the same when deriving the
  // new value to return.
  return oldValue &- n
}

// Relaxed atomic add. Returns the old value.
@discardableResult
fileprivate func addRelaxed(_ atomic: UnsafeMutablePointer<Int>, n: Int) -> Int {
  return Int(Builtin.atomicrmw_add_monotonic_Word(atomic._rawValue, n._builtinWordValue))
}

// Atomic add with release ordering. Returns the old value.
@discardableResult
fileprivate func addRelease(_ atomic: UnsafeMutablePointer<Int>, n: Int) -> Int {
  return Int(Builtin.atomicrmw_add_release_Word(atomic._rawValue, n._builtinWordValue))
}

// Compare-and-swap with relaxed ordering. Returns a tuple containing the old value, and whether the operation succeeded.
fileprivate func compareExchangeRelaxed(_ atomic: UnsafeMutablePointer<Int>, expectedOldValue: Int, desiredNewValue: Int) -> (oldValue: Int, won: Bool) {
  let (oldValue, won) = Builtin.cmpxchg_monotonic_monotonic_Word(atomic._rawValue, expectedOldValue._builtinWordValue, desiredNewValue._builtinWordValue)
  return (Int(oldValue), Bool(won))
}

fileprivate func storeRelease(_ atomic: UnsafeMutablePointer<Int>, newValue: Int) {
  Builtin.atomicstore_release_Word(atomic._rawValue, newValue._builtinWordValue)
}

fileprivate func storeRelaxed(_ atomic: UnsafeMutablePointer<Int>, newValue: Int) {
  Builtin.atomicstore_monotonic_Word(atomic._rawValue, newValue._builtinWordValue)
}

#if _pointerBitWidth(_64)

/// Weak and unowned references
///
/// See Weak Reference Design at the top of the file for details on this implementation.

// The type for the implementation of a weak reference value. It's still a
// pointer to the weakly-referenced object, but managed differently.
typealias WeakReference = UnsafeMutablePointer<HeapObject>

// A weak reference slot is a pointer to the actual in-memory representation of
// a weak reference, which may be nil. The weak entrypoints operate on slots
// rather than the values directly.
typealias WeakSlot = UnsafeMutablePointer<WeakReference?>

// Form an optional object pointer from raw pointer bits.
func optionalObject(_ bits: Builtin.RawPointer) -> UnsafeMutablePointer<HeapObject>? {
  if UInt(Builtin.ptrtoint_Word(bits)) == 0 {
    return nil
  }
  return unsafe UnsafeMutablePointer<HeapObject>(bits)
}

// Initialize the uninitialized slot `ref` to `value`, which may be nil, and
// return `ref`. `value` is passed at +0 and gains a weak reference.
@c
public func swift_weakInit(ref: Builtin.RawPointer, value: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_weakInit(ref: WeakSlot(ref), value: optionalObject(value))._rawValue
}

func swift_weakInit(ref: WeakSlot, value: UnsafeMutablePointer<HeapObject>?) -> WeakSlot {
  if unsafe objectIsLiveForWeakReference(object: value) {
    unsafe weakRetain(object: value)
    unsafe ref.pointee = value
  } else {
    unsafe ref.pointee = nil
  }
  return unsafe ref
}

// Assign `value`, which may be nil, to the initialized slot `ref`, and return
// `ref`. `value` is passed at +0 and gains a weak reference. The slot's old
// value loses a weak reference.
@c
public func swift_weakAssign(ref: Builtin.RawPointer, value: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_weakAssign(ref: WeakSlot(ref), value: optionalObject(value))._rawValue
}

func swift_weakAssign(ref: WeakSlot, value: UnsafeMutablePointer<HeapObject>?) -> WeakSlot {
  var newValue: UnsafeMutablePointer<HeapObject>? = nil
  if unsafe objectIsLiveForWeakReference(object: value) {
    unsafe weakRetain(object: value)
    unsafe newValue = value
  }
  unsafe weakRelease(object: ref.pointee)
  unsafe ref.pointee = newValue
  return unsafe ref
}

// Return the object referenced by the initialized slot `ref` at +1, or nil if
// it has been deallocated or the slot contains nil.
@c
public func swift_weakLoadStrong(ref: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe Builtin.reinterpretCast(swift_weakLoadStrong(ref: WeakSlot(ref)))
}

func swift_weakLoadStrong(ref: WeakSlot) -> UnsafeMutablePointer<HeapObject>? {
  guard let object = unsafe ref.pointee else { return nil }
  return unsafe weakLoad(object: object)
}

// Return the object referenced by the initialized slot `ref` at +1, or nil if
// it has been deallocated or the slot contains nil. `ref` is left
// uninitialized, consuming the weak reference it held.
@c
public func swift_weakTakeStrong(ref: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe Builtin.reinterpretCast(swift_weakTakeStrong(ref: WeakSlot(ref)))
}

func swift_weakTakeStrong(ref: WeakSlot) -> UnsafeMutablePointer<HeapObject>? {
  let object = unsafe swift_weakLoadStrong(ref: ref)
  unsafe weakRelease(object: ref.pointee)
  return unsafe object
}

// Consume the weak reference in `ref`, leaving it uninitialized.
@c
public func swift_weakDestroy(ref: Builtin.RawPointer) {
  unsafe swift_weakDestroy(ref: WeakSlot(ref))
}

func swift_weakDestroy(ref: WeakSlot) {
  unsafe weakRelease(object: ref.pointee)
}

// Initialize the uninitialized slot `dest` from `src`, and return `dest`. The
// referenced object gains a weak reference.
@c
public func swift_weakCopyInit(dest: Builtin.RawPointer, src: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_weakCopyInit(dest: WeakSlot(dest), src: WeakSlot(src))._rawValue
}

func swift_weakCopyInit(dest: WeakSlot, src: WeakSlot) -> WeakSlot {
  let object = unsafe src.pointee

  // If the object is dead then store nil into dest as an optimization.
  if unsafe !objectIsLiveForWeakReference(object: object) {
    unsafe dest.pointee = nil
  } else {
    unsafe weakRetain(object: object)
    unsafe dest.pointee = object
  }
  return unsafe dest
}

// Initialize the uninitialized slot `dest` by moving the weak reference out of
// the initialized slot `src`, leaving `src` uninitialized, and return `dest`.
// No refcounts change.
@c
public func swift_weakTakeInit(dest: Builtin.RawPointer, src: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_weakTakeInit(dest: WeakSlot(dest), src: WeakSlot(src))._rawValue
}

func swift_weakTakeInit(dest: WeakSlot, src: WeakSlot) -> WeakSlot {
  unsafe dest.pointee = src.pointee
  return unsafe dest
}

// Assign into the initialized slot `dest` from the initialized slot `src`, and
// return `dest`. The object `src` references gains a weak reference, `dest`'s
// old value loses one, and `src` keeps the one it held.
@c
public func swift_weakCopyAssign(dest: Builtin.RawPointer, src: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_weakCopyAssign(dest: WeakSlot(dest), src: WeakSlot(src))._rawValue
}

func swift_weakCopyAssign(dest: WeakSlot, src: WeakSlot) -> WeakSlot {
  return unsafe swift_weakAssign(ref: dest, value: src.pointee)
}

// Assign into the initialized slot `dest` by moving the weak reference out of
// the initialized slot `src`, leaving `src` uninitialized, and return `dest`.
// `dest`'s old value loses a weak reference.
@c
public func swift_weakTakeAssign(dest: Builtin.RawPointer, src: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_weakTakeAssign(dest: WeakSlot(dest), src: WeakSlot(src))._rawValue
}

func swift_weakTakeAssign(dest: WeakSlot, src: WeakSlot) -> WeakSlot {
  if unsafe dest != src {
    unsafe weakRelease(object: dest.pointee)
    unsafe dest.pointee = src.pointee
  }
  return unsafe dest
}

// Unowned references are loadable under native refcounting, so IRGen emits
// these value operations rather than the address operations weak references
// use. The swift_unownedInit / Assign / LoadStrong / Destroy / CopyInit / ...
// names in the C++ runtime are inline wrappers around these four and are never
// emitted as calls.

// Add an unowned reference to `object`, which may be nil, and return it.
// Unowned references share the weak refcount here, so this is a weak retain.
@c
@discardableResult
public func swift_unownedRetain(object: Builtin.RawPointer) -> Builtin.RawPointer {
  unsafe swift_unownedRetain(object: optionalObject(object))
  return object
}

// IRGen emits unowned_retain on the payload of an `unowned var x: T?`, which is
// nil when the reference is, so nil is not an error here.
func swift_unownedRetain(object: UnsafeMutablePointer<HeapObject>?) {
  guard let object = unsafe object else { return }
  if unsafe !objectIsLiveForWeakReference(object: object) {
    fatalError("unowned retain of a dead object")
  }
  unsafe weakRetain(object: object)
}

// Remove an unowned reference from `object`, which may be nil,
// deallocating it if that was the last reference of any kind.
@c
public func swift_unownedRelease(object: Builtin.RawPointer) {
  unsafe swift_unownedRelease(object: optionalObject(object))
}

func swift_unownedRelease(object: UnsafeMutablePointer<HeapObject>?) {
  unsafe weakRelease(object: object)
}

// Return `object`, which may be nil, at +1 strong, raising a fatal error if
// it has already been deallocated. The unowned reference the caller holds is
// unchanged.
@c
@discardableResult
public func swift_unownedRetainStrong(object: Builtin.RawPointer) -> Builtin.RawPointer {
  unsafe swift_unownedRetainStrong(object: optionalObject(object))
  return object
}

func swift_unownedRetainStrong(object: UnsafeMutablePointer<HeapObject>?) {
  guard let object = unsafe object else { return }
  if unsafe weakLoad(object: object) == nil {
    fatalError("load of an unowned reference to a dead object")
  }
}

// Add a strong reference to `object`, if non-nil, and remove the caller's
// unowned reference, raising a fatal error if `object` has already been
// deallocated.
@c
public func swift_unownedRetainStrongAndRelease(object: Builtin.RawPointer) {
  unsafe swift_unownedRetainStrongAndRelease(object: optionalObject(object))
}

func swift_unownedRetainStrongAndRelease(object: UnsafeMutablePointer<HeapObject>?) {
  unsafe swift_unownedRetainStrong(object: object)
  unsafe swift_unownedRelease(object: object)
}

// Raise a fatal error if `object`, if non-nil, has already been deallocated. No
// refcounts change.
@c
public func swift_unownedCheck(object: Builtin.RawPointer) {
  unsafe swift_unownedCheck(object: optionalObject(object))
}

func swift_unownedCheck(object: UnsafeMutablePointer<HeapObject>?) {
  guard let object = unsafe object else { return }
  if unsafe !objectIsLiveForWeakReference(object: object) {
    fatalError("load of an unowned reference to a dead object")
  }
}

// swift_unownedRetain, without atomicity.
@c
@discardableResult
public func swift_nonatomic_unownedRetain(object: Builtin.RawPointer) -> Builtin.RawPointer {
  return swift_unownedRetain(object: object)
}

// swift_unownedRelease, without atomicity.
@c
public func swift_nonatomic_unownedRelease(object: Builtin.RawPointer) {
  swift_unownedRelease(object: object)
}

// swift_unownedRetainStrong, without atomicity.
@c
@discardableResult
public func swift_nonatomic_unownedRetainStrong(object: Builtin.RawPointer) -> Builtin.RawPointer {
  return swift_unownedRetainStrong(object: object)
}

// swift_unownedRetainStrongAndRelease, without atomicity.
@c
public func swift_nonatomic_unownedRetainStrongAndRelease(object: Builtin.RawPointer) {
  swift_unownedRetainStrongAndRelease(object: object)
}

// Increment the weak reference count of the target of a weak reference.
func weakRetain(object: WeakReference?) {
  guard let object = unsafe object else { return }

  let refcount = unsafe refcountPointer(for: object)

  // The weak refcount exists to track when an object can be freed. If it's
  // never freed, there's no need to track it.
  if unsafe loadRelaxed(refcount) & HeapObject.doNotFreeBit != 0 {
    return
  }

  let oldValue = unsafe addRelaxed(refcount, n: HeapObject.weakRefcountOne)
  if (oldValue & HeapObject.weakRefcountMask) == HeapObject.weakRefcountMax {
    fatalError("weak reference count overflow")
  }
}

// Load a weak reference. Atomically retain the object and return it, or return
// nil if the target has started deinit.
func weakLoad(object: WeakReference) -> UnsafeMutablePointer<HeapObject>? {
  // The liveness test and the retain must be a single atomic operation.
  // tryRetain does this, and returns the refcount value which we can check for
  // liveness.
  let refcountValue = unsafe tryRetain(object: object)
  return unsafe refcountValueIsLiveForWeakReference(refcountValue) ? object : nil
}

// Decrement the weak reference count of the target of a weak reference. If this
// is the last weak reference on the object, deallocate it.
//
// When called from swift_deallocClassInstance, the object's size and alignment
// are provided. Other callers don't have those values. Deallocation requires
// them, so the call from swift_deallocClassInstance stashes them in the
// now-unused metadata field of the HeapObject. That call is guaranteed to take
// place before the object is deallocated, so the values are always available.
//
// The malloc type id is not currently preserved that way, and is only passed
// through when the dealloc happens in the call from
// swift_deallocClassInstanceTyped. Platforms that need the type id to be
// provided to dealloc must not use weak references.
func weakRelease(object: WeakReference?, allocatedSize: Int? = nil, allocatedAlignMask: Int = 0, typeId: UInt64 = 0) {
  guard let object = unsafe object else { return }

  let refcount = unsafe refcountPointer(for: object)
  let refcountValue = unsafe loadRelaxed(refcount)

  // The weak refcount exists to track when an object can be freed. If it's
  // never freed, there's no need to track that.
  if refcountValue & HeapObject.doNotFreeBit != 0 {
    return
  }

  // Perform the actual decrement.
  let oldValue: Int
  if let allocatedSize {
    if refcountValue & HeapObject.weakRefcountMask == 0 {
      // Common case fast path: this is the call from swift_deallocClassInstance
      // and this is the last/only weak reference. We have the size and
      // alignment mask available. Deallocate and return.
      if typeId != 0 {
        swift_deallocObjectTyped(object: object._rawValue, allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask, typeId: typeId)
      } else {
        unsafe swift_deallocObject(object: object, allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
      }
      return
    } else {
      // This is the call from swift_deallocClassInstance, which provides the size
      // and alignment. This was not the last weak reference (although it may
      // become the last one by the time we do the atomic subtract below), so we
      // need to store this size/alignment in the now-unused metadata field for
      // the last weakRelease to use. Size and alignment are both less than 32
      // bits, so they'll fit in the 64-bit metadata field.
      let sizeAndAlignment = (allocatedSize << 32) | allocatedAlignMask
      unsafe object.pointee.metadata = UnsafeRawPointer(bitPattern: sizeAndAlignment)

      // Ensure that this is visible by decrementing the weak refcount with
      // release ordering. This pairs with the acquire below.
      oldValue = unsafe addRelease(refcount, n: -HeapObject.weakRefcountOne)
    }
  } else {
    oldValue = unsafe addRelaxed(refcount, n: -HeapObject.weakRefcountOne)
  }

  // If the old weak refcount was 0, then this is the last weak reference and
  // it's time to free the object. The subtraction above underflowed and
  // wrapped, potentially damaging other parts of the refcount field, but
  // nothing else can have a reference to this object at this point without
  // unsafe code.
  if (oldValue & HeapObject.weakRefcountMask) == 0 {
    if let allocatedSize {
      // If the caller passed in a size/alignment, we can pass those through.
      // This only happens if we didn't take the common case fast path above,
      // but ended up doing the last weak release anyway.
      if typeId != 0 {
        swift_deallocObjectTyped(object: object._rawValue, allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask, typeId: typeId)
      } else {
        unsafe swift_deallocObject(object: object, allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
      }
    } else {
      // Extract the size/alignment that the call from
      // swift_deallocClassInstance stashed in the metadata pointer refcount,
      // and use it. swift_deallocClassInstance always does this, and if we're
      // at refcount 0 then swift_deallocClassInstance must have been called
      // previously, and stashed the values.

      // Ensure that the write to the metadata field is visible to us by doing a
      // load acquire on the refcount field. This pairs with the addRelease
      // above.
      _ = unsafe loadAcquire(refcount)
      let sizeAndAlignment = Int(bitPattern: unsafe object.pointee.metadata)
      let size = (sizeAndAlignment >> 32) & ((1 << 32) - 1)
      let alignMask = sizeAndAlignment & ((1 << 32) - 1)
      unsafe swift_deallocObject(object: object, allocatedSize: size, allocatedAlignMask: alignMask)
    }
  }
}

// Return whether an object is live and able to be the target of a weak
// reference. If false, the object has already begun deinit. When true, the
// result is inherently racy, since another thread could start deinit. A true
// result must handle this gracefully.
func objectIsLiveForWeakReference(object: UnsafeMutablePointer<HeapObject>?) -> Bool {
  guard let object = unsafe object else { return false }

  let refcount = unsafe refcountPointer(for: object)
  let refcountValue = unsafe loadRelaxed(refcount)
  return refcountValueIsLiveForWeakReference(refcountValue)
}

// The refcount-value half of objectIsLiveForWeakReference, for callers that
// already have the value in hand. Pairs with tryRetain, which returns the
// refcount value it acted on.
func refcountValueIsLiveForWeakReference(_ refcountValue: Int) -> Bool {
  // Static objects are always live.
  if refcountValue == HeapObject.staticRefCount {
    return true
  }

  // Non-static objects with doNotFreeBit set are stack objects. These can never
  // be the target of a weak reference while live. If a weak reference could be
  // created while an object is live, the compiler won't stack-promote it. A
  // weak reference can be created in deinit without blocking stack promotion,
  // but then the object is no longer live. Note that immortalRefCount is not
  // reliably set for stack objects, so this can't be rolled into the check
  // below.
  if (refcountValue & HeapObject.doNotFreeBit) != 0 {
    return false
  }

  // A heap object is live until its deinit runs, which swift_release_n_ marks
  // by storing immortalRefCount.
  return (refcountValue & HeapObject.refcountMask) != HeapObject.immortalRefCount
}

#endif // _pointerBitWidth(_64)

// Once

@c
public func swift_once(predicate: UnsafeMutablePointer<Int>, fn: (@convention(c) (UnsafeMutableRawPointer)->()), context: UnsafeMutableRawPointer) {
  let checkedLoadAcquire = { predicate in
    let value = unsafe loadAcquire(predicate)
    assert(value == -1 || value == 0 || value == 1)
    return value
  }

  if unsafe checkedLoadAcquire(predicate) < 0 { return }

  let won = unsafe compareExchangeRelaxed(predicate, expectedOldValue: 0, desiredNewValue: 1).1
  if won {
    unsafe fn(context)
    unsafe storeRelease(predicate, newValue: -1)
    return
  }

  // TODO: This should really use an OS provided lock
  while unsafe checkedLoadAcquire(predicate) >= 0 {
    // spin
  }
}



// Misc

@c
public func swift_deletedMethodError() -> Never {
  Builtin.int_trap()
}

@_silgen_name("swift_willThrow") // This is actually expected to be swiftcc (@_silgen_name and not @c).
public func swift_willThrow() throws {
}

/// Called when a typed error will be thrown.
@_silgen_name("swift_willThrowTyped")
public func _willThrowTyped<E: Error>(_ error: E) {
}

#if !SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
// The Embedded Swift platform abstraction layer uses separate entrypoints.

public func swift_stdlib_random(_ buf: UnsafeMutableRawPointer, _ nbytes: Int) {
#if os(Linux) && !SWIFT_STDLIB_HAS_ARC4RANDOM
  let EINTR: CInt = 4
  var buf = unsafe buf
  var remaining = nbytes
  while remaining > 0 {
    let count = unsafe getrandom(buf, remaining, 0)
    if count <= 0 {
      // A signal can interrupt the call while it waits for the entropy pool to
      // be seeded. Every other failure means there is no entropy source, and
      // handing back a buffer that was never filled would silently produce
      // predictable values.
      if count < 0, unsafe __errno_location().pointee == EINTR { continue }
      fatalError("unable to obtain entropy from getrandom")
    }
    unsafe buf += count
    remaining -= count
  }
#else
  unsafe arc4random_buf(buf: buf, nbytes: nbytes)
#endif
}
#endif

@c
@inline(never)
public func swift_clearSensitive(buf: UnsafeMutableRawPointer, nbytes: Int) {
  // TODO: use memset_s if available
  // Though, it shouldn't make too much difference because the `@inline(never)` should prevent
  // the optimizer from removing the loop below.
  let bytePtr = unsafe buf.assumingMemoryBound(to: UInt8.self)
  for i in 0..<nbytes {
    unsafe bytePtr[i] = 0
  }
}

@inline(never)
func _embeddedReportFatalError(prefix: StaticString, message: StaticString) {
  print(prefix, terminator: "")
  if message.utf8CodeUnitCount > 0 { print(": ", terminator: "") }
  print(message)
}

@inline(never)
func _embeddedReportFatalError(prefix: StaticString, message: UnsafeBufferPointer<UInt8>) {
  print(prefix, terminator: "")
  if message.count > 0 { print(": ", terminator: "") }
  unsafe print(message)
}

@inline(never)
@usableFromInline
func _embeddedReportFatalErrorInFile(prefix: StaticString, message: StaticString, file: StaticString, line: UInt) {
  print(file, terminator: ":")
  print(line, terminator: ": ")
  print(prefix, terminator: "")
  if message.utf8CodeUnitCount > 0 { print(": ", terminator: "") }
  print(message)
}

@inline(never)
@usableFromInline
func _embeddedReportFatalErrorInFile(prefix: StaticString, message: UnsafeBufferPointer<UInt8>, file: StaticString, line: UInt) {
  print(file, terminator: ":")
  print(line, terminator: ": ")
  print(prefix, terminator: "")
  if message.count > 0 { print(": ", terminator: "") }
  unsafe print(message)
}

// Error-kind-based variants. On platforms that provide the Embedded Swift
// platform layer, the numeric `kind` is passed through to `_swift_reportError`
// so the platform can format the error itself; otherwise the prefix is printed.
// These are `@usableFromInline` (referenced by the emitted-into-client
// `_assertionFailure` entrypoints) but kept out of line so the cold reporting
// path isn't inlined into every call site.

@usableFromInline
@inline(never)
func _embeddedReportFatalError(kind: Int, message: StaticString) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  message.withUTF8Buffer { messageBuffer in
    unsafe _swift_reportError(
      messageBuffer.baseAddress, messageBuffer.count, CUnsignedLongLong(kind))
  }
#else
  print(kind._failureMessagePrefix(), terminator: "")
  if message.utf8CodeUnitCount > 0 { print(": ", terminator: "") }
  print(message)
#endif
}

@usableFromInline
@inline(never)
func _embeddedReportFatalError(kind: Int, message: UnsafeBufferPointer<UInt8>) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  unsafe _swift_reportError(
    message.baseAddress, message.count, CUnsignedLongLong(kind))
#else
  print(kind._failureMessagePrefix(), terminator: "")
  if message.count > 0 { print(": ", terminator: "") }
  unsafe print(message)
#endif
}

@usableFromInline
@inline(never)
func _embeddedReportFatalErrorInFile(kind: Int, message: StaticString, file: StaticString, line: UInt) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  message.withUTF8Buffer { messageBuffer in
    file.withUTF8Buffer { fileBuffer in
      unsafe _swift_reportErrorAt(
        messageBuffer.baseAddress, messageBuffer.count,
        fileBuffer.baseAddress, fileBuffer.count, Int(line), CUnsignedLongLong(kind))
    }
  }
#else
  print(file, terminator: ":")
  print(line, terminator: ": ")
  print(kind._failureMessagePrefix(), terminator: "")
  if message.utf8CodeUnitCount > 0 { print(": ", terminator: "") }
  print(message)
#endif
}

@usableFromInline
@inline(never)
func _embeddedReportFatalErrorInFile(kind: Int, message: UnsafeBufferPointer<UInt8>, file: StaticString, line: UInt) {
#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
  file.withUTF8Buffer { fileBuffer in
    unsafe _swift_reportErrorAt(
      message.baseAddress, message.count,
      fileBuffer.baseAddress, fileBuffer.count, Int(line), CUnsignedLongLong(kind))
  }
#else
  print(file, terminator: ":")
  print(line, terminator: ": ")
  print(kind._failureMessagePrefix(), terminator: "")
  if message.count > 0 { print(": ", terminator: "") }
  unsafe print(message)
#endif
}

extension Access.Action {
  func printName() {
    switch self {
    case .read:
      print("read", terminator: "")
    case .modify:
      print("modify", terminator: "")
    }
  }
}

@inline(never)
func _embeddedReportExclusivityViolation(
  oldAction: Access.Action, oldPC: UnsafeRawPointer?,
  newAction: Access.Action, newPC: UnsafeRawPointer?,
  pointer: UnsafeRawPointer
) {
  if _isDebugAssertConfiguration() {
    print("Simultaneous access to 0x", terminator: "")
    printAsHex(Int(bitPattern: pointer), terminator: "")
    print(", but modification requires exclusive access")

    print("Previous access (a ", terminator: "")
    oldAction.printName()
    print(") started at 0x", terminator: "")
    printAsHex(Int(bitPattern: oldPC))

    print("Current access (a ", terminator: "")
    newAction.printName()
    print(") started at 0x", terminator: "")
    printAsHex(Int(bitPattern: newPC))
  }
  Builtin.condfail_message(
    true._value, StaticString("dynamic exclusivity violation").unsafeRawPointer)
  Builtin.int_trap()
}

@_extern(c)
func _swift_tls_get(_ key: Int) -> UnsafeMutableRawPointer?

@_extern(c)
func _swift_tls_set(_ key: Int, _ pointer: UnsafeMutableRawPointer?)

@c
func _swift_getExclusivityTLS() -> UnsafeMutableRawPointer? {
  return unsafe _swift_tls_get(/*exclusivity=*/7)
}

@c
func _swift_setExclusivityTLS(_ pointer: UnsafeMutableRawPointer?) {
  return unsafe _swift_tls_set(/*exclusivity=*/7, pointer)
}

private func intToFloatChunkFactor<T: ExpressibleByFloatLiteral>() -> T {
#if _pointerBitWidth(_64)
  return 0x1p64
#elseif _pointerBitWidth(_32)
  return 0x1p32
#else
#warning("Unsupported platform")
  fatalError()
#endif
}

// IntegerLiteral-to-FloatingPoint conversion

/// Convert a `Builtin.IntegerLiteral` value to a binary floating-point type.
///
/// `data` is a pointer to little-endian word-sized chunks; `flags` packs the
/// minimum bit width needed to store the value (in bits 8 and above) and the
/// sign (in bit 0). The format mirrors `swift::IntegerLiteral` /
/// `swift::IntegerLiteralFlags` in `include/swift/Runtime/Numeric.h` and
/// `include/swift/ABI/MetadataValues.h`. The same algorithm is implemented
/// in C++ in `stdlib/public/runtime/Numeric.cpp` for non-Embedded builds.
@_silgen_name("swift_intToFloat32")
public func _swift_intToFloat32(
  _ data: UnsafePointer<UInt>, _ flags: Int
) -> Float {
  let bitsPerChunk = Int.bitWidth
  let bitWidth = flags >> 8
  let numChunks = (bitWidth + bitsPerChunk - 1) / bitsPerChunk

  // Single chunk: the entire value is sign-extended into one chunk.
  if numChunks == 1 {
    return Float(Int(bitPattern: unsafe data[0]))
  }

  // Multi-chunk: lower chunks contribute as unsigned digits in base
  // 2^bitsPerChunk; only the top chunk carries the sign.
  let chunkFactor: Float = intToFloatChunkFactor()
  var result = Float(unsafe data[0])
  var scale = chunkFactor
  for i in 1 ..< numChunks - 1 {
    result += Float(unsafe data[i]) * scale
    scale *= chunkFactor
  }
  result += Float(Int(bitPattern: unsafe data[numChunks - 1])) * scale
  return result
}

@_silgen_name("swift_intToFloat64")
public func _swift_intToFloat64(
  _ data: UnsafePointer<UInt>, _ flags: Int
) -> Double {
  let bitsPerChunk = Int.bitWidth
  let bitWidth = flags >> 8
  let numChunks = (bitWidth + bitsPerChunk - 1) / bitsPerChunk

  if numChunks == 1 {
    return Double(Int(bitPattern: unsafe data[0]))
  }

  let chunkFactor: Double = intToFloatChunkFactor()
  var result = Double(unsafe data[0])
  var scale = chunkFactor
  for i in 1 ..< numChunks - 1 {
    result += Double(unsafe data[i]) * scale
    scale *= chunkFactor
  }
  result += Double(Int(bitPattern: unsafe data[numChunks - 1])) * scale
  return result
}

// Version information for the Platform Abstraction Layer

#if SWIFT_USE_EMBEDDED_SWIFT_PLATFORM
@c @used
public func swift_getPlatformLayerVersion(
  _ major: UnsafeMutablePointer<Int>,
  _ minor: UnsafeMutablePointer<Int>
) {
  unsafe major.pointee = 1 // EMBEDDED_SWIFT_PLATFORM_VERSION_MAJOR
  unsafe minor.pointee = 1 // EMBEDDED_SWIFT_PLATFORM_VERSION_MINOR
}
#endif

// CXX Exception Personality

public typealias _Unwind_Action = CInt
public typealias _Unwind_Reason_Code = CInt

@c @used
public func _swift_exceptionPersonality(
  version: CInt,
  actions: _Unwind_Action,
  exceptionClass: UInt64,
  exceptionObject: UnsafeMutableRawPointer,
  context: UnsafeMutableRawPointer
) -> _Unwind_Reason_Code {
  fatalError("C++ exception handling detected but the Embedded Swift runtime does not support exceptions")
}
