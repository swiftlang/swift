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
      case ._cocoa(let object): return ObjectIdentifier(object)
      case ._native(let object): return ObjectIdentifier(object)
      default: return nil
    }
  }
}

extension String {
  public // @testable
  func _classify() -> _StringRepresentation { return _guts._classify() }
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
    if _object.largeIsCocoa {
      result._form = ._cocoa(object: _object.cocoaObject)
      return result
    }

    // TODO: shared native
    _internalInvariant(_object.providesFastUTF8)
    _internalInvariant(_object.largeFastIsNative)
    if _object.isImmortal {
      result._form = ._immortal(
        address: UInt(bitPattern: _object.nativeUTF8Start))
      return result
    }
    if _object.hasNativeStorage {
      result._form = ._native(object: _object.nativeStorage)
      return result
    }
    fatalError()
  }
}

