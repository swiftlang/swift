let _swift_MinAllocationAlignment: UInt = 16

@_silgen_name("posix_memalign")
func posix_memalign(_: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _: UInt, _: UInt) -> CInt

@_silgen_name("free")
func free(_: UnsafeMutableRawPointer?)

func alignedAlloc(size: UInt, alignment: UInt) -> UnsafeMutableRawPointer? {
  let alignment = max(alignment, UInt(MemoryLayout<UnsafeRawPointer>.size))
  var r: UnsafeMutableRawPointer? = nil
  _ = posix_memalign(&r, alignment, size)
  return r
}

/// Public APIs
// void *swift_slowAlloc(size_t size, size_t alignMask);
@_cdecl("swift_slowAlloc")
public func swift_slowAlloc(_ size: UInt, _ alignMask: UInt) -> UnsafeMutableRawPointer? {
  let alignment: UInt
  if alignMask == UInt.max {
    alignment = _swift_MinAllocationAlignment
  } else {
    alignment = alignMask + 1
  }
  return alignedAlloc(size: size, alignment: alignment)
}

// void swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask);
@_cdecl("swift_slowDealloc")
public func swift_slowDealloc(_ ptr: UnsafeMutableRawPointer?, _ size: UInt, _ alignMask: UInt) {
  free(ptr)
}

@_silgen_name("swift_allocObject")
public func swift_allocObject(metadata: UnsafeMutableRawPointer, requiredSize: UInt, requiredAlignmentMask: UInt) -> UnsafeMutablePointer<HeapObject> {
  let p = swift_slowAlloc(requiredSize, requiredAlignmentMask)!
  let object = p.assumingMemoryBound(to: HeapObject.self)
  object.pointee.metadata = metadata
  object.pointee.refcount = 1
  return object
}

@_silgen_name("swift_deallocClassInstance")
public func swift_deallocClassInstance(object: UnsafeMutablePointer<HeapObject>, allocatedSize: UInt, allocatedAlignMask: UInt) {
  free(object)
}

@_silgen_name("swift_initStackObject")
public func swift_initStackObject(metadata: UnsafeMutableRawPointer, object: UnsafeMutablePointer<HeapObject>) -> UnsafeMutablePointer<HeapObject> {
  object.pointee.metadata = metadata
  object.pointee.refcount = -1
  return object
}

@_silgen_name("swift_isUniquelyReferenced_nonNull_native")
public func swift_isUniquelyReferenced_nonNull_native(object: UnsafeMutablePointer<HeapObject>) -> Bool {
  return object.pointee.refcount == 1
}

public struct HeapObject {
  var metadata: UnsafeMutableRawPointer
  var refcount: Int
}

@_silgen_name("swift_retain")
public func swift_retain(object: UnsafeMutablePointer<HeapObject>) -> UnsafeMutablePointer<HeapObject> {
  if object.pointee.refcount == -1 { return object }
  object.pointee.refcount += 1
  return object
}

@_silgen_name("swift_release")
public func swift_release(object: UnsafeMutablePointer<HeapObject>) {
  if object.pointee.refcount == -1 { return }
  object.pointee.refcount -= 1
  if object.pointee.refcount == 0 {
    free(object)
  }
}
