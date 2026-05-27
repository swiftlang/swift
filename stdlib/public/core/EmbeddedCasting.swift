//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

struct DynamicCastFlags {
  static let Default           = UInt(bitPattern: 0x0)
  static let Unconditional     = UInt(bitPattern: 0x1)
  static let TakeOnSuccess     = UInt(bitPattern: 0x2)
  static let DestroyOnFailure  = UInt(bitPattern: 0x4)
}

struct ValueWitnessFlags {
  static let IsNonInline = UInt32(0x00020000)
}

/// The representation of an existential type, which describes its layout it
/// memory.
enum ExistentialTypeRepresentation: UInt8 {
  /// An opaque existential, which has a 3-word buffer followed by the type
  /// metadata.
  case opaque = 0

  /// A class-bound existential, which stores the object.
  case `class` = 1

  /// An error existential, i.e., `any Error`, which stores a boxed error.
  case error = 2

  /// Given an address of an existential value that has the representation
  /// described by `self`, project to the stored metadata and value.
  func project(
    _ existentialRaw: UnsafeRawPointer
  ) -> (metadata: UnsafeRawPointer, value: UnsafeRawPointer) {
    // Extract the metadata pointer.
    let existential = unsafe existentialRaw.assumingMemoryBound(to: UnsafeRawPointer.self)
    switch self {
    case .opaque:
      // For an opaque existential, there is a 3-word buffer followed by the
      // metadata. If the value is stored out-of-line, word 0 holds a pointer to
      // a heap box instead.
      let storedMetadata = unsafe existential[3]
      return unsafe (
        metadata: storedMetadata,
        value: projectOpaqueValue(existentialRaw, storedMetadata: storedMetadata)
      )

    case .class:
      /// For a class-bound existential, the payload is the object itself and
      /// the metadata is available within the object.
      let object = unsafe existential[0]
      return unsafe (
        metadata: unsafe object.assumingMemoryBound(to: UnsafeRawPointer.self)
          .pointee,
        value: object
      )
    
    case .error:
      /// For an 'any Error' existential, we have an error box containing the
      /// metadata and the value.
      let (type, _, value) = unsafe _errorBoxContents(UnsafeRawPointer(existential[0]))
      return unsafe (metadata: type, value: value)
    }
  }

  /// Project the value address from an opaque existential, handling both
  /// inline storage and non-inline (heap-boxed) storage.
  private func projectOpaqueValue(
    _ existentialRaw: UnsafeRawPointer,
    storedMetadata: UnsafeRawPointer
  ) -> UnsafeRawPointer {
    let flags = unsafe UInt32(_swift_embedded_metadata_get_vwt_flags(
        UnsafeMutableRawPointer(mutating: storedMetadata)))
    if flags & ValueWitnessFlags.IsNonInline != 0 {
      // Non-inline: word 0 is a box pointer; project through to the value.
      let box = unsafe existentialRaw.assumingMemoryBound(to: UnsafeRawPointer.self)[0]
      let alignMask = Int(unsafe _swift_embedded_metadata_get_align_mask(
        UnsafeMutableRawPointer(mutating: storedMetadata)))
      let startOfValue = unsafe (MemoryLayout<HeapObject>.size + alignMask) & ~alignMask
      return unsafe box.advanced(by: startOfValue)
    }
    // Inline: value starts at word 0 of the existential buffer.
    return unsafe existentialRaw
  }
}

/// Describes the different kinds of metadata.
enum MetadataKind: Equatable {
  /// The type is a class.
  case `class`

  /// The type is a struct.
  case `struct`

  /// The type is an enum.
  case `enum`

  /// The type is an existential with the given representation.
  case existential(ExistentialTypeRepresentation)

  /// Given a pointer to metadata, figure out its kind.
  init?(metadata: UnsafeRawPointer) {
    // The kinds here match those in swift/ABI/MetadataKind.def.
    let addrOfMetadataKind = unsafe metadata.assumingMemoryBound(to: UInt.self)
    let kind = unsafe addrOfMetadataKind.pointee
    switch kind {
    case 0: self = .class
    case 0 | Self.metadataKindIsNonHeap: self = .struct
    case 1 | Self.metadataKindIsNonHeap: self = .enum
 
    case 3 | Self.metadataKindIsNonHeap | Self.metadataKindIsRuntimePrivate:
      let rawRepr = unsafe (addrOfMetadataKind + 1).pointee
      if let rawRepr8 = UInt8(exactly: rawRepr),
         let repr = ExistentialTypeRepresentation(rawValue: rawRepr8) {
        self = .existential(repr)
      } else {
        return nil
      }
 
    default:
      if kind > Self.lastEnumerated {
        self = .class
      } else {
        return nil
      }
    }
  }

  /// Metadata that represents something that isn't a type.
  static let metadataKindIsNonType: UInt = 0x400

  /// Metadata that represents something that isn't always stored on the heap.
  static let metadataKindIsNonHeap: UInt = 0x200

  /// Metadata that is private to the ABI-stable runtime.
  static let metadataKindIsRuntimePrivate: UInt = 0x100

  /// The last enumerated value for the metadata. Any value greater than this is
  /// considered a class.
  static let lastEnumerated = 0x7FF
}

func projectHeapObject(_ exist: UnsafeRawPointer) -> UnsafeRawPointer{
  return unsafe exist.assumingMemoryBound(to: UnsafeRawPointer.self).pointee
}

/// The result of performing a dynamic cast.
private enum DynamicCastResult {
  /// The cast failed.
  case failure

  /// The cast succeeded by copying the source. The source is still valid.
  case successViaCopy

  /// The cast succeeded by taking from the source. The source is now
  /// uninitialized.
  case successViaTake
}

/// Attempt to perform a dynamic cast from src to dst, where we have information
/// about the metadata for each.
private func tryCast(
  dst: UnsafeMutableRawPointer,
  dstMetadata: UnsafeRawPointer,
  src: UnsafeRawPointer,
  srcMetadata: UnsafeRawPointer,
  takeOnSuccess: Bool,
) -> DynamicCastResult {
  // If types match exactly, the cast succeeds. We just move/copy the data.
  if unsafe srcMetadata == dstMetadata {
    switch takeOnSuccess {
    case true:
      unsafe _swift_embedded_metadata_initialize_with_take(
        srcMetadata,
        dst,
        UnsafeMutableRawPointer(mutating: src)
      )
      return .successViaTake

    case false:
      unsafe _swift_embedded_metadata_initialize_with_copy(
        srcMetadata,
        dst,
        src
      )
      return .successViaCopy
    }
  }

  // If we can't figure out the source metadata kind, only the exact match
  // above can result in a successful cast, so fail quickly.
  guard let srcMetadataKind = unsafe MetadataKind(metadata: srcMetadata) else {
    return .failure
  }

  switch srcMetadataKind {
  case .existential(let repr):
    // Project the existential, then try to cast it.
    // Note that we can only "take" on success if the projection didn't change
    // the address. Otherwise, we would be destroying an inner object.
    let (srcInnerMetadata, srcInner) = unsafe repr.project(src)
    return unsafe tryCast(
      dst: dst,
      dstMetadata: dstMetadata,
      src: srcInner,
      srcMetadata: srcInnerMetadata,
      takeOnSuccess: takeOnSuccess && src == srcInner
    )

  case .class:
    // If the destination is not a class, this cannot succeed.
    guard unsafe MetadataKind(metadata: dstMetadata) == .class else {
      return .failure
    }

    // Attempt to perform the class downcast.
    let srcObject = unsafe projectHeapObject(src)
    guard let resultObject = unsafe swift_dynamicCastClass(
      object: srcObject,
      targetMetadata: dstMetadata
    ) else {
      return .failure
    }

    // Success! Write the resulting object into the destination.
    unsafe UnsafeMutableRawPointer(mutating: dst)
        .assumingMemoryBound(to: UnsafeRawPointer.self).pointee = resultObject

    // Adjust reference counting if needed.
    switch takeOnSuccess {
    case true:
      return .successViaTake

    case false:
      // The dynamic cast operation is +0, so perform an extra retain on the
      // result object when we were asked to copy.
      swift_retain(object: resultObject._rawValue)
      return .successViaCopy
    }

  default:
    // We handled equivalence above. There are no other subtyping relations
    // permitted in Embedded Swift.
    return .failure
  }
}

@c
public func swift_dynamicCast(
  _ dest: Builtin.RawPointer, /* points to a concrete type */
  _ src: Builtin.RawPointer, /* points to an existential or class */
  _ srcMetadata: Builtin.RawPointer,
  _ dstMetadata: Builtin.RawPointer,
  _ flags: UInt
) -> Bool {

  let isUnconditionalCast : Bool = (flags & DynamicCastFlags.Unconditional) != 0
  let isTakeOnSuccess : Bool = (flags & DynamicCastFlags.TakeOnSuccess) != 0
  let isDestroyOnFailure : Bool =
    (flags & DynamicCastFlags.DestroyOnFailure) != 0

  let castResult = unsafe tryCast(
    dst: UnsafeMutableRawPointer(dest),
    dstMetadata: UnsafeRawPointer(dstMetadata),
    src: UnsafeRawPointer(src),
    srcMetadata: UnsafeRawPointer(srcMetadata),
    takeOnSuccess: isTakeOnSuccess
  )

  switch castResult {
  case .failure:
    if isUnconditionalCast {
      fatalError("failed cast")
    }

    if isDestroyOnFailure {
      unsafe _swift_embedded_metadata_destroy(
        UnsafeRawPointer(srcMetadata),
        UnsafeMutableRawPointer(src)
      )
    }

    return false

  case .successViaCopy:
    // If we were asked to do a take, but ended up having to copy, destroy the
    // original source.
    if isTakeOnSuccess {
      unsafe _swift_embedded_metadata_destroy(
        UnsafeRawPointer(srcMetadata),
        UnsafeMutableRawPointer(src)
      )
    }

    return true

  case .successViaTake:
    return true
  }
}

@c
public func swift_dynamicCastClass(
  object: UnsafeRawPointer,
  targetMetadata: UnsafeRawPointer
) -> UnsafeRawPointer? {
  let sourceObj = unsafe object.assumingMemoryBound(to: HeapObject.self)
  var type = unsafe _swift_embedded_get_heap_object_metadata_pointer(
    UnsafeMutableRawPointer(mutating: sourceObj)
  ).assumingMemoryBound(to: ClassMetadata.self)
  let targetType = unsafe targetMetadata.assumingMemoryBound(to: ClassMetadata.self)
  while unsafe type != targetType {
    guard let superType = unsafe type.pointee.superclassMetadata else {
      return nil
    }
    unsafe type = UnsafePointer(superType)
  }
  return unsafe object
}

@c
public func swift_dynamicCastClassUnconditional(object: UnsafeMutableRawPointer, targetMetadata: UnsafeRawPointer,
    file: UnsafePointer<CChar>, line: CUnsignedInt, column: CUnsignedInt) -> UnsafeRawPointer {
  guard let result = unsafe swift_dynamicCastClass(object: object, targetMetadata: targetMetadata) else {
    fatalError("failed cast")
  }
  return unsafe result
}
