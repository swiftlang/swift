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
  var superclassMetadata: UnsafeMutablePointer<ClassMetadata>?

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

  The scheme for storing and maintaining a refcount on heap objects is very simple in Embedded Swift, and is much
  simpler than regular Swift's. This is mainly due to the fact that we currently only maintain the regular ("strong")
  refcount and we don't allow weak references, unowned references and we don't track refcount during deinit of the
  object.

  The refcount is always stored directly inline in the heap object, in the `refcount` field (see HeapObject struct
  below). This field has the following structure (on 32-bit, and similar on other bitwidths):

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
  off the doNotFreeBit.

  The actual number of references has one possible value that has a special meaning, immortalRefCount (all bits set,
  i.e. 0x7fff_ffff on 32-bit systems). When used, retain and release operations do nothing, references are not counted,
  and the object can never be deinit'd / free'd. This is used for class instances that are promoted by the compiler to
  be allocated statically in global memory (see swift_initStaticObject). Note that there are two different scenarios for
  this currently:

  - In most cases, a class instance that is promoted to a global, is still dynamically initialized with a runtime call
    to swift_initStaticObject. This function will set the refcount field to immortalRefCount | doNotFreeBit.
  - As a special case to allow arrays be fully statically initialized without runtime overhead, instances of
    _ContiguousArrayStorage can be promoted to __StaticArrayStorage with the HeapObject header emitted directly by the
    compiler and refcount field directly set to immortalRefCount | doNotFreeBit (see irgen::emitConstantObject).

  The immortalRefCount is additionally also used as a placeholder value for objects (heap-allocated or stack-allocated)
  when they're currently inside their deinit(). This is done to prevent further retains and releases inside deinit from
  triggering deinitialization again, without the need to reserve another bit for this purpose. Retains and releases in
  deinit() are allowed, as long as they are balanced at the end, i.e. the object is not escaped (user's responsibility)
  and not over-released (this can only be caused by unsafe code).

  The following table summarizes the meaning of the possible combinations of doNotFreeBit and have immortal refcount
  value:

  ┌───────────╥──────────╥─────────────────────────────────────────────────┐
  │ doNotFree ║ immortal ║                                                 │
  ╞═══════════╬══════════╬═════════════════════════════════════════════════╡
  │ 0         ║ no       ║ regular class instance                          │
  ├───────────╫──────────╫─────────────────────────────────────────────────┤
  │ 0         ║ yes      ║ regular class instance during deinit()          │
  ├───────────╫──────────╫─────────────────────────────────────────────────┤
  │ 1         ║ no       ║ stack-allocated                                 │
  ├───────────╫──────────╫─────────────────────────────────────────────────┤
  │ 1         ║ yes      ║ global-allocated, no need to track references,  │
  │           ║          ║ or stack-allocated instance during deinit()     │
  └───────────╨──────────╨─────────────────────────────────────────────────┘
*/
@unsafe
public struct HeapObject {
  // There is no way to express the custom ptrauth signature on the metadata
  // field, so let's use UnsafeRawPointer and a helper function in C instead
  // (_swift_embedded_set_heap_object_metadata_pointer).
  var metadata: UnsafeRawPointer

  // TODO: This is just an initial support for strong refcounting only. We need
  // to think about supporting (or banning) weak and/or unowned references.
  var refcount: Int

  // Note: The immortalRefCount value is also hard-coded in IRGen in `irgen::emitConstantObject`, and in HeapObject.h.
#if _pointerBitWidth(_64)
  static let doNotFreeBit     = Int(bitPattern: 0x8000_0000_0000_0000)
  static let refcountMask     = Int(bitPattern: 0x7fff_ffff_ffff_ffff)
  static let immortalRefCount = Int(bitPattern: 0x7fff_ffff_ffff_ffff) // Make sure we don't have doNotFreeBit set
#elseif _pointerBitWidth(_32)
  static let doNotFreeBit     = Int(bitPattern: 0x8000_0000)
  static let refcountMask     = Int(bitPattern: 0x7fff_ffff)
  static let immortalRefCount = Int(bitPattern: 0x7fff_ffff) // Make sure we don't have doNotFreeBit set
#elseif _pointerBitWidth(_16)
  static let doNotFreeBit     = Int(bitPattern: 0x8000)
  static let refcountMask     = Int(bitPattern: 0x7fff)
  static let immortalRefCount = Int(bitPattern: 0x7fff) // Make sure we don't have doNotFreeBit set
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

@_extern(c, "posix_memalign")
func posix_memalign(_: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _: Int, _: Int) -> CInt

@_extern(c, "free")
func free(_ p: UnsafeMutableRawPointer?)



/// Allocations

func alignedAlloc(size: Int, alignment: Int) -> UnsafeMutableRawPointer? {
  let alignment = max(alignment, unsafe MemoryLayout<UnsafeRawPointer>.size)
  var r: UnsafeMutableRawPointer? = nil
  _ = unsafe posix_memalign(&r, alignment, size)
  return unsafe r
}

@c
public func swift_coroFrameAlloc(_ size: Int, _ type: UInt64) -> UnsafeMutableRawPointer? {
  return unsafe alignedAlloc(
    size: size,
    alignment: _swift_MinAllocationAlignment)
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
  unsafe free(ptr)
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
public func swift_deallocUninitializedObject(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocObject(
    object: UnsafeMutablePointer<HeapObject>(object),
    allocatedSize: allocatedSize,
    allocatedAlignMask: allocatedAlignMask)
}

@c
public func swift_deallocObject(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocObject(object: UnsafeMutablePointer<HeapObject>(object), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
}

func swift_deallocObject(object: UnsafeMutablePointer<HeapObject>, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe free(UnsafeMutableRawPointer(object))
}

@c
public func swift_deallocClassInstance(object: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocClassInstance(object: UnsafeMutablePointer<HeapObject>(object), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
}

func swift_deallocClassInstance(object: UnsafeMutablePointer<HeapObject>, allocatedSize: Int, allocatedAlignMask: Int) {
  if (unsafe object.pointee.refcount & HeapObject.doNotFreeBit) != 0 {
    return
  }

  unsafe free(UnsafeMutableRawPointer(object))
}

@c
public func swift_deallocPartialClassInstance(object: Builtin.RawPointer, metadata: Builtin.RawPointer, allocatedSize: Int, allocatedAlignMask: Int) {
  unsafe swift_deallocPartialClassInstance(object: UnsafeMutablePointer<HeapObject>(object), metadata: UnsafeMutablePointer<ClassMetadata>(metadata), allocatedSize: allocatedSize, allocatedAlignMask: allocatedAlignMask)
}

func swift_deallocPartialClassInstance(object: UnsafeMutablePointer<HeapObject>, metadata: UnsafeMutablePointer<ClassMetadata>, allocatedSize: Int, allocatedAlignMask: Int) {
  var classMetadata = unsafe _swift_embedded_get_heap_object_metadata_pointer(object).assumingMemoryBound(to: ClassMetadata.self)
  while unsafe classMetadata != metadata {
    unsafe _swift_embedded_invoke_heap_object_optional_ivardestroyer(object, classMetadata)
    guard let superclassMetadata = unsafe classMetadata.pointee.superclassMetadata else { break }
    unsafe classMetadata = superclassMetadata
  }
}

@c
public func swift_initStaticObject(metadata: Builtin.RawPointer, object: Builtin.RawPointer) -> Builtin.RawPointer {
  return unsafe swift_initStaticObject(metadata: UnsafeMutablePointer<ClassMetadata>(metadata), object: UnsafeMutablePointer<HeapObject>(object))._rawValue
}

func swift_initStaticObject(metadata: UnsafeMutablePointer<ClassMetadata>, object: UnsafeMutablePointer<HeapObject>) -> UnsafeMutablePointer<HeapObject> {
  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, metadata)
  unsafe object.pointee.refcount = HeapObject.immortalRefCount | HeapObject.doNotFreeBit
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
public var _emptyBoxStorage: (Int, Int) = (/*isa*/0, /*refcount*/-1)

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

@_silgen_name("swift_allocBox")
public func swift_allocBox(_ metadata: Builtin.RawPointer) -> (Builtin.RawPointer, Builtin.RawPointer) {
  let alignMask = Int(unsafe _swift_embedded_metadata_get_align_mask(UnsafeMutableRawPointer(metadata)))
  let size = Int(unsafe _swift_embedded_metadata_get_size(UnsafeMutableRawPointer(metadata)))
  let headerSize = unsafe MemoryLayout<Int>.size + MemoryLayout<UnsafeRawPointer>.size
  let headerAlignMask = unsafe MemoryLayout<UnsafeRawPointer>.alignment - 1
  let startOfBoxedValue = ((headerSize + alignMask) & ~alignMask)
  let requiredSize: Int = startOfBoxedValue + size
  let requiredAlignmentMask: Int = alignMask | headerAlignMask

  let p = unsafe swift_slowAlloc(requiredSize, requiredAlignmentMask)!
  let object = unsafe p.assumingMemoryBound(to: HeapObject.self)

  unsafe _swift_embedded_set_heap_object_metadata_pointer(object, UnsafeMutableRawPointer(metadata))
  unsafe object.pointee.refcount = 1

  let boxedValueAddr = unsafe UnsafeMutableRawPointer(p).advanced(by: startOfBoxedValue)

  return (object._rawValue, boxedValueAddr._rawValue)
}

@c
public func swift_deallocBox(_ object: Builtin.RawPointer) {
  unsafe free(UnsafeMutableRawPointer(object))
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
/// Dynamic cast support
/// Only supports existential container to concrete casts.
struct DynamicCastFlags {
  static let Default           = UInt(bitPattern: 0x0)
  static let Unconditional     = UInt(bitPattern: 0x1)
  static let TakeOnSuccess     = UInt(bitPattern: 0x2)
  static let DestroyOnFailure  = UInt(bitPattern: 0x4)
}

struct MetadataKind {
  static let LastEnumerated = UInt(bitPattern: 0x7FF)
}

func projectExistentialMetadata(
  _ exist: Builtin.RawPointer
) -> Builtin.RawPointer {

  let offset = (3 * MemoryLayout<Builtin.RawPointer>.size)
  let addrOfMetadata = unsafe UnsafeMutableRawPointer(exist) + offset
  let metadataPtrAddr = unsafe addrOfMetadata.bindMemory(to: Builtin.RawPointer.self, capacity: 1)
  return unsafe metadataPtrAddr.pointee
}

func isClassMetadata(_ metadata: Builtin.RawPointer) -> Bool {
  let addrOfMetadataKind = unsafe UnsafePointer<UInt>(metadata)
  let kind = unsafe addrOfMetadataKind.pointee
  return  kind == 0 || kind > MetadataKind.LastEnumerated
}

func projectHeapObject(_ exist: Builtin.RawPointer) -> UnsafeMutableRawPointer{
  let addrOfHeapObject = unsafe UnsafePointer<UnsafeMutableRawPointer>(exist)
  return unsafe addrOfHeapObject.pointee
}

@c
public func swift_dynamicCast(
  _ dest: Builtin.RawPointer, /* points to a concrete type */
  _ src: Builtin.RawPointer, /* points to an existential */
  _ srcMetadata: Builtin.RawPointer, /* always nullptr */
  _ dstMetadata: Builtin.RawPointer,
  _ flags: UInt
) -> Bool {

  let isUnconditionalCast : Bool = (flags & DynamicCastFlags.Unconditional) != 0
  let isTakeOnSuccess : Bool = (flags & DynamicCastFlags.TakeOnSuccess) != 0
  let isDestroyOnFailure : Bool =
    (flags & DynamicCastFlags.DestroyOnFailure) != 0

  let srcMetadata = projectExistentialMetadata(src)

  let isClass = isClassMetadata(dstMetadata)

  if isClass {
    var success = false
    let obj = unsafe projectHeapObject(src)
    if isClassMetadata(srcMetadata) {
      if srcMetadata != dstMetadata {
        // check parent chain
        success = unsafe swift_dynamicCastClass(object: obj, targetMetadata: UnsafeMutableRawPointer(dstMetadata)) != nil
      } else {
        success = true
      }
    }
    if !success {
      if isDestroyOnFailure {
        unsafe _swift_embedded_existential_destroy(UnsafeMutableRawPointer(src),
                                                   swift_releaseBox)
      }

      if isUnconditionalCast {
        fatalError("failed cast")
      }
      return false
    }
    if isTakeOnSuccess {
      let dst = unsafe UnsafeMutablePointer<UnsafeMutableRawPointer>(dest)
      unsafe dst.pointee = obj

    } else {
      let dst = unsafe UnsafeMutablePointer<UnsafeMutableRawPointer>(dest)
      unsafe dst.pointee = obj
      swift_retain(object: obj._rawValue)
    }
    return true;
  }
  // destintation type is not a class. Test exact match.
  let success = srcMetadata == dstMetadata
  if !success {
    if isDestroyOnFailure {
      unsafe _swift_embedded_existential_destroy(UnsafeMutableRawPointer(src),
                                                 swift_releaseBox)
    }

    if isUnconditionalCast {
      fatalError("failed cast")
    }
    return false
  }
  if isTakeOnSuccess {
    // take from an existential to a concrete type
    unsafe _swift_embedded_existential_init_with_take(
             UnsafeMutableRawPointer(dest), UnsafeMutableRawPointer(src),
             swift_releaseBox)
  } else {
    // copy from an existential to a concrete type
    unsafe _swift_embedded_existential_init_with_copy(
             UnsafeMutableRawPointer(dest), UnsafeMutableRawPointer(src))
  }
  return true
}

/// Refcounting

func isValidPointerForNativeRetain(object: Builtin.RawPointer) -> Bool {
  let objectBits = UInt(Builtin.ptrtoint_Word(object))
  if objectBits == 0 { return false }

  #if _pointerBitWidth(_64)
  if unsafe (objectBits & HeapObject.immortalObjectPointerBit) != 0 { return false }
  #endif

  return true
}

@c
public func swift_setDeallocating(object: Builtin.RawPointer) {
}

@c
public func swift_dynamicCastClass(object: UnsafeMutableRawPointer, targetMetadata: UnsafeRawPointer) -> UnsafeMutableRawPointer? {
  let sourceObj = unsafe object.assumingMemoryBound(to: HeapObject.self)
  var type = unsafe _swift_embedded_get_heap_object_metadata_pointer(sourceObj).assumingMemoryBound(to: ClassMetadata.self)
  let targetType = unsafe targetMetadata.assumingMemoryBound(to: ClassMetadata.self)
  while unsafe type != targetType {
    guard let superType = unsafe type.pointee.superclassMetadata else {
      return nil
    }
    unsafe type = UnsafeMutablePointer(superType)
  }
  return unsafe object
}

@c
public func swift_dynamicCastClassUnconditional(object: UnsafeMutableRawPointer, targetMetadata: UnsafeRawPointer,
    file: UnsafePointer<CChar>, line: CUnsignedInt, column: CUnsignedInt) -> UnsafeMutableRawPointer {
  guard let result = unsafe swift_dynamicCastClass(object: object, targetMetadata: targetMetadata) else {
    fatalError("failed cast")
  }
  return unsafe result
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
  return unsafe loadAcquire(refcount) == 1
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

  unsafe addRelaxed(refcount, n: Int(n))

  return unsafe object
}

@c
@discardableResult
public func swift_bridgeObjectRetain(object: Builtin.RawPointer) -> Builtin.RawPointer {
  return swift_bridgeObjectRetain_n(object: object, n: 1)
}

@c
public func swift_bridgeObjectRetain_n(object: Builtin.RawPointer, n: UInt32) -> Builtin.RawPointer {
  let objectBits = UInt(Builtin.ptrtoint_Word(object))
  let untaggedObject = unsafe Builtin.inttoptr_Word((objectBits & HeapObject.bridgeObjectToPlainObjectMask)._builtinWordValue)
  return swift_retain_n(object: untaggedObject, n: n)
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

func swift_release_n_(object: UnsafeMutablePointer<HeapObject>?, n: UInt32, isBoxRelease: Bool = false) {
  guard let object = unsafe object else {
    return
  }

  let refcount = unsafe refcountPointer(for: object)
  let loadedRefcount = unsafe loadRelaxed(refcount)
  if unsafe loadedRefcount & HeapObject.refcountMask == HeapObject.immortalRefCount {
    return
  }

  let resultingRefcount = unsafe subFetchAcquireRelease(refcount, n: Int(n)) & HeapObject.refcountMask
  if resultingRefcount == 0 {
    // Set the refcount to immortalRefCount before calling the object destroyer
    // to prevent future retains/releases from having any effect. Unlike the
    // full Swift runtime, we don't track the refcount inside deinit, so we
    // won't be able to detect escapes or over-releases of `self` in deinit. We
    // might want to reconsider that in the future.
    //
    // There can only be one thread with a reference at this point (because
    // we're releasing the last existing reference), so a relaxed store is
    // enough.
    let doNotFree = unsafe (loadedRefcount & HeapObject.doNotFreeBit) != 0
    unsafe storeRelaxed(refcount, newValue: HeapObject.immortalRefCount | (doNotFree ? HeapObject.doNotFreeBit : 0))

    if isBoxRelease {
        unsafe _swift_embedded_invoke_box_destroy(object)
    } else {
        unsafe _swift_embedded_invoke_heap_object_destroy(object)
    }
  } else if resultingRefcount < 0 {
    fatalError("negative refcount")
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
public func swift_bridgeObjectRelease(object: Builtin.RawPointer) {
  swift_bridgeObjectRelease_n(object: object, n: 1)
}

@c
public func swift_bridgeObjectRelease_n(object: Builtin.RawPointer, n: UInt32) {
  let objectBits = UInt(Builtin.ptrtoint_Word(object))
  let untaggedObject = unsafe Builtin.inttoptr_Word((objectBits & HeapObject.bridgeObjectToPlainObjectMask)._builtinWordValue)
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
  return oldValue - n
}

fileprivate func addRelaxed(_ atomic: UnsafeMutablePointer<Int>, n: Int) {
  _ = Builtin.atomicrmw_add_monotonic_Word(atomic._rawValue, n._builtinWordValue)
}

fileprivate func compareExchangeRelaxed(_ atomic: UnsafeMutablePointer<Int>, expectedOldValue: Int, desiredNewValue: Int) -> Bool {
  let (_, won) = Builtin.cmpxchg_monotonic_monotonic_Word(atomic._rawValue, expectedOldValue._builtinWordValue, desiredNewValue._builtinWordValue)
  return Bool(won)
}

fileprivate func storeRelease(_ atomic: UnsafeMutablePointer<Int>, newValue: Int) {
  Builtin.atomicstore_release_Word(atomic._rawValue, newValue._builtinWordValue)
}

fileprivate func storeRelaxed(_ atomic: UnsafeMutablePointer<Int>, newValue: Int) {
  Builtin.atomicstore_monotonic_Word(atomic._rawValue, newValue._builtinWordValue)
}

// Once

@c
public func swift_once(predicate: UnsafeMutablePointer<Int>, fn: (@convention(c) (UnsafeMutableRawPointer)->()), context: UnsafeMutableRawPointer) {
  let checkedLoadAcquire = { predicate in
    let value = unsafe loadAcquire(predicate)
    assert(value == -1 || value == 0 || value == 1)
    return value
  }

  if unsafe checkedLoadAcquire(predicate) < 0 { return }

  let won = unsafe compareExchangeRelaxed(predicate, expectedOldValue: 0, desiredNewValue: 1)
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

@_extern(c, "arc4random_buf")
func arc4random_buf(buf: UnsafeMutableRawPointer, nbytes: Int)

public func swift_stdlib_random(_ buf: UnsafeMutableRawPointer, _ nbytes: Int) {
  unsafe arc4random_buf(buf: buf, nbytes: nbytes)
}

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
