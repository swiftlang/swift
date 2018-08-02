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
  func _classify() -> _StringRepresentation {
    var result = _StringRepresentation(
      _isASCII: _guts._isASCIIOrSmallASCII,
      _count: _guts.count,
      _capacity: _guts.capacity,
      _form: ._small
    )
    if _guts._isSmall {
      return result
    }
    if _guts._isNative {
      result._form = ._native(object: _guts._owner!)
      return result
    }
    if _guts._isCocoa {
      result._form = ._cocoa(object: _guts._owner!)
      return result
    }
    if _guts._isUnmanaged {
      result._form = ._immortal(
        address: UInt(bitPattern: _guts._unmanagedRawStart))
      return result
    }
    fatalError()
  }
}

