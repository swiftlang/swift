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

// Declarations to enable ease-of-testing

public // @testable
struct _StringRepresentation {
  public var _isASCII: Bool
  public var _count: Int
  public var _capacity: Int

  #if $Embedded
    public typealias AnyObject = Builtin.NativeObject
  #endif

  public enum _Form {
    case _small
    case _cocoa(object: AnyObject)
    case _native(object: AnyObject)
    case _immortal(address: UInt)
    // TODO: shared native
  }
  public var _form: _Form

  public var _objectIdentifier: ObjectIdentifier? {
    switch _form {
      case ._cocoa(let object):
        #if !$Embedded
        return ObjectIdentifier(object)
        #else
        return ObjectIdentifier(_nativeObject(toNative: object))
        #endif
      case ._native(let object):
        #if !$Embedded
        return ObjectIdentifier(object)
        #else
        return ObjectIdentifier(_nativeObject(toNative: object))
        #endif
      default: return nil
    }
  }
}

@available(*, unavailable)
extension _StringRepresentation: Sendable {}

@available(*, unavailable)
extension _StringRepresentation._Form: Sendable {}

extension String {
  public // @testable
  func _classify() -> _StringRepresentation { return _guts._classify() }

#if !$Embedded
  @_alwaysEmitIntoClient
  public // @testable
  func _deconstructUTF8<ToPointer: _Pointer>(
    scratch: UnsafeMutableRawBufferPointer?
  ) -> (
    owner: AnyObject?,
    ToPointer,
    length: Int,
    usesScratch: Bool,
    allocatedMemory: Bool
  ) {
    _guts._deconstructUTF8(scratch: scratch)
  }
#endif
}

extension _StringGuts {
  internal func _classify() -> _StringRepresentation {
    var result = _StringRepresentation(
      _isASCII: self.isASCII,
      _count: self.count,
      _capacity: nativeCapacity ?? 0,
      _form: ._small
    )
    if self.isSmall {
      result._capacity = _SmallString.capacity
      return result
    }
    #if !$Embedded
    if _object.largeIsCocoa {
      result._form = ._cocoa(object: _object.cocoaObject)
      return result
    }
    #endif

    // TODO: shared native
    _internalInvariant(_object.providesFastUTF8)
    if _object.isImmortal {
      result._form = ._immortal(
        address: UInt(bitPattern: _object.nativeUTF8Start))
      return result
    }
    if _object.hasNativeStorage {
      _internalInvariant(_object.largeFastIsTailAllocated)
      #if !$Embedded
      result._form = ._native(object: _object.nativeStorage)
      #else
      result._form = ._native(object: Builtin.unsafeCastToNativeObject(_object.nativeStorage))
      #endif
      return result
    }
    fatalError()
  }

#if !$Embedded

/*

 Deconstruct the string into contiguous UTF-8, allocating memory if necessary

┌────────────────────╥───────────────────────┬─────────────────────┬─────────────┬─────────────────┐
│ Form               ║ owner                 │ pointer+length      │ usesScratch │ allocatedMemory │
├────────────────────╫───────────────────────┼─────────────────────┼─────────────┼─────────────────┤
│ small with scratch ║ nil                   │ `scratch`           │ true        │ false           │
├────────────────────╫───────────────────────┼─────────────────────┼─────────────┼─────────────────┤
│ small w/o scratch  ║ extra allocation      │ `owner` pointer     │ false       │ true            │
╞════════════════════╬═══════════════════════╪═════════════════════╪═════════════╪═════════════════╡
│ immortal, large    ║ nil                   │ literal pointer     │ false       │ false           │
├────────────────────╫───────────────────────┼─────────────────────┼─────────────┼─────────────────┤
│ native             ║ __StringStorage       │ tail alloc pointer  │ false       │ false           │
╞════════════════════╬═══════════════════════╪═════════════════════╪═════════════╪═════════════════╡
│ shared             ║ __SharedStringStorage │ shared pointer      │ false       │ false           │
├────────────────────╫───────────────────────┼─────────────────────┼─────────────┼─────────────────┤
│ shared, bridged    ║ _CocoaString          │ cocoa ASCII pointer │ false       │ false           │
╞════════════════════╬═══════════════════════╪═════════════════════╪═════════════╪═════════════════╡
│ foreign            ║ extra allocation      │ `owner` pointer     │ false       │ true            │
└────────────────────╨───────────────────────┴─────────────────────┴─────────────┴─────────────────┘

*/
  @_alwaysEmitIntoClient
  internal // TODO: figure out if this works as a compiler intrinsic
  func _deconstructUTF8<ToPointer: _Pointer>(
    scratch: UnsafeMutableRawBufferPointer?
  ) -> (
    owner: AnyObject?,
    ToPointer,
    length: Int,
    usesScratch: Bool,
    allocatedMemory: Bool
  ) {

    // If we're small, try to copy into the scratch space provided
    if self.isSmall {
      let smol = self.asSmall
      if let scratch = scratch, scratch.count > smol.count {
        let scratchStart =
          scratch.baseAddress!
        smol.withUTF8 { smolUTF8 -> () in
          scratchStart.initializeMemory(
            as: UInt8.self, from: smolUTF8.baseAddress!, count: smolUTF8.count)
        }
        scratch[smol.count] = 0
        return (
          owner: nil,
          _convertPointerToPointerArgument(scratchStart),
          length: smol.count,
          usesScratch: true, allocatedMemory: false)
      }
    } else if _fastPath(self.isFastUTF8) {
      let ptr: ToPointer =
        _convertPointerToPointerArgument(self._object.fastUTF8.baseAddress!)
      return (
        owner: self._object.owner,
        ptr,
        length: self._object.count,
        usesScratch: false, allocatedMemory: false)
    }

    let (object, ptr, len) = self._allocateForDeconstruct()
    return (
      owner: object,
      _convertPointerToPointerArgument(ptr),
      length: len,
      usesScratch: false,
      allocatedMemory: true)
  }

  @_alwaysEmitIntoClient
  @inline(never) // slow path
  internal
  func _allocateForDeconstruct() -> (
    owner: AnyObject,
    UnsafeRawPointer,
    length: Int
  ) {
    let utf8 = Array(String(self).utf8) + [0]
    let (owner, ptr): (AnyObject?, UnsafeRawPointer) =
      _convertConstArrayToPointerArgument(utf8)

    // Array's owner cannot be nil, even though it is declared optional...
    return (owner: owner!, ptr, length: utf8.count - 1)
  }

#endif

}
