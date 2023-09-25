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

public struct ClassMetadata {
  var superclassMetadata: UnsafeMutablePointer<ClassMetadata>?

  // There is no way to express the actual calling convention on the heap desroy
  // function (swiftcc with 'self') currently, so let's use UnsafeRawPointer
  // and a helper function in C (_swift_runtime_invoke_heap_object_destroy).
  var destroy: UnsafeRawPointer
}

public struct HeapObject {
  var metadata: UnsafeMutablePointer<ClassMetadata>
  var refcount: Int
}

func alignedAlloc(size: Int, alignment: Int) -> UnsafeMutableRawPointer? {
  let alignment = max(alignment, MemoryLayout<UnsafeRawPointer>.size)
  var r: UnsafeMutableRawPointer? = nil
  _ = posix_memalign(&r, alignment, size)
  return r
}

@_cdecl("swift_slowAlloc")
public func swift_slowAlloc(_ size: Int, _ alignMask: Int) -> UnsafeMutableRawPointer? {
  let alignment: Int
  if alignMask == -1 {
    alignment = _swift_MinAllocationAlignment
  } else {
    alignment = alignMask + 1
  }
  return alignedAlloc(size: size, alignment: alignment)
}

@_cdecl("swift_slowDealloc")
public func swift_slowDealloc(_ ptr: UnsafeMutableRawPointer?, _ size: Int, _ alignMask: Int) {
  free(ptr)
}

@_silgen_name("swift_allocObject")
public func swift_allocObject(metadata: UnsafeMutablePointer<ClassMetadata>, requiredSize: Int, requiredAlignmentMask: Int) -> UnsafeMutablePointer<HeapObject> {
  let p = swift_slowAlloc(requiredSize, requiredAlignmentMask)!
  let object = p.assumingMemoryBound(to: HeapObject.self)
  object.pointee.metadata = metadata
  object.pointee.refcount = 1
  return object
}

@_silgen_name("swift_deallocClassInstance")
public func swift_deallocClassInstance(object: UnsafeMutablePointer<HeapObject>, allocatedSize: Int, allocatedAlignMask: Int) {
  free(object)
}

@_silgen_name("swift_initStackObject")
public func swift_initStackObject(metadata: UnsafeMutablePointer<ClassMetadata>, object: UnsafeMutablePointer<HeapObject>) -> UnsafeMutablePointer<HeapObject> {
  object.pointee.metadata = metadata
  object.pointee.refcount = -1
  return object
}

// TODO/FIXME: Refcounting and swift_once is not thread-safe, the following only works in single-threaded environments.

@_silgen_name("swift_isUniquelyReferenced_nonNull_native")
public func swift_isUniquelyReferenced_nonNull_native(object: UnsafeMutablePointer<HeapObject>) -> Bool {
  // TODO/FIXME: Refcounting is not thread-safe, the following only works in single-threaded environments.
  return object.pointee.refcount == 1
}

@_silgen_name("swift_retain")
public func swift_retain(object: Builtin.RawPointer) -> Builtin.RawPointer {
  if Int(Builtin.ptrtoint_Word(object)) == 0 { return object }
  let o = UnsafeMutablePointer<HeapObject>(object)
  // TODO/FIXME: Refcounting is not thread-safe, the following only works in single-threaded environments.
  if o.pointee.refcount == -1 { return o._rawValue }
  o.pointee.refcount += 1
  return o._rawValue
}

@_silgen_name("swift_release")
public func swift_release(object: Builtin.RawPointer) {
  if Int(Builtin.ptrtoint_Word(object)) == 0 { return }
  let o = UnsafeMutablePointer<HeapObject>(object)
  // TODO/FIXME: Refcounting is not thread-safe, the following only works in single-threaded environments.
  if o.pointee.refcount == -1 { return }
  o.pointee.refcount -= 1
  if o.pointee.refcount == 0 {
    _swift_runtime_invoke_heap_object_destroy(o.pointee.metadata.pointee.destroy, o)
  }
}

@_silgen_name("swift_beginAccess")
public func swift_beginAccess(pointer: UnsafeMutableRawPointer, buffer: UnsafeMutableRawPointer, flags: UInt, pc: UnsafeMutableRawPointer) {
}

@_silgen_name("swift_endAccess")
public func swift_endAccess(buffer: UnsafeMutableRawPointer) {
}

@_silgen_name("swift_once")
public func swift_once(predicate: UnsafeMutablePointer<Int>, fn: (@convention(c) (UnsafeMutableRawPointer)->()), context: UnsafeMutableRawPointer) {
  // TODO/FIXME: The following only works in single-threaded environments.
  if predicate.pointee == 0 {
    predicate.pointee = 1
    fn(context)
  }
}
