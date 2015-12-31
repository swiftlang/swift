//===--- ObjCMirrors.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

#if _runtime(_ObjC)
@_silgen_name("swift_ObjCMirror_count") 
func _getObjCCount(_: _MagicMirrorData) -> Int
@_silgen_name("swift_ObjCMirror_subscript") 
func _getObjCChild(_: Int, _: _MagicMirrorData) -> (String, _MirrorType)

func _getObjCSummary(data: _MagicMirrorData) -> String {
  let theDescription = _swift_stdlib_objcDebugDescription(data._loadValue())
  return _cocoaStringToSwiftString_NonASCII(theDescription)
}

public // SPI(runtime)
struct _ObjCMirror : _MirrorType {
  let data: _MagicMirrorData

  public var value: Any { return data.objcValue }
  public var valueType: Any.Type { return data.objcValueType }
  public var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  public var count: Int {
    return _getObjCCount(data)
  }
  public subscript(i: Int) -> (String, _MirrorType) {
    return _getObjCChild(i, data)
  }
  public var summary: String {
    return _getObjCSummary(data)
  }
  public var quickLookObject: PlaygroundQuickLook? {
    return _getClassPlaygroundQuickLook(data)
  }
  public var disposition: _MirrorDisposition { return .ObjCObject }
}

public // SPI(runtime)
struct _ObjCSuperMirror : _MirrorType {
  let data: _MagicMirrorData

  public var value: Any { return data.objcValue }
  public var valueType: Any.Type { return data.objcValueType }

  // Suppress the value identifier for super mirrors.
  public var objectIdentifier: ObjectIdentifier? {
    return nil
  }
  public var count: Int {
    return _getObjCCount(data)
  }
  public subscript(i: Int) -> (String, _MirrorType) {
    return _getObjCChild(i, data)
  }
  public var summary: String {
    return _getObjCSummary(data)
  }
  public var quickLookObject: PlaygroundQuickLook? {
    return _getClassPlaygroundQuickLook(data)
  }
  public var disposition: _MirrorDisposition { return .ObjCObject }
}
#endif
