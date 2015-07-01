//===--- ObjCMirrors.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
@asmname("swift_ObjCMirror_count") 
func _getObjCCount(_: _MagicMirrorData) -> Int
@asmname("swift_ObjCMirror_subscript") 
func _getObjCChild(_: Int, _: _MagicMirrorData) -> (String, MirrorType)

@objc protocol _DebugDescriptionProxy {
  var debugDescription: _CocoaStringType {get}
}

func _getObjCSummary(data: _MagicMirrorData) -> String {
  let theObject = data._loadValue() as _DebugDescriptionProxy
  return _cocoaStringToSwiftString_NonASCII(theObject.debugDescription)
}

public // SPI(runtime)
struct _ObjCMirror : MirrorType {
  let data: _MagicMirrorData

  public var value: Any { return data.objcValue }
  public var valueType: Any.Type { return data.objcValueType }
  public var objectIdentifier: ObjectIdentifier? {
    return data._loadValue() as ObjectIdentifier
  }
  public var count: Int {
    return _getObjCCount(data)
  }
  public subscript(i: Int) -> (String, MirrorType) {
    return _getObjCChild(i, data)
  }
  public var summary: String {
    return _getObjCSummary(data)
  }
  public var quickLookObject: PlaygroundQuickLook? {
    return _getClassPlaygroundQuickLook(data)
  }
  public var disposition: MirrorDisposition { return .ObjCObject }
}

public // SPI(runtime)
struct _ObjCSuperMirror : MirrorType {
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
  public subscript(i: Int) -> (String, MirrorType) {
    return _getObjCChild(i, data)
  }
  public var summary: String {
    return _getObjCSummary(data)
  }
  public var quickLookObject: PlaygroundQuickLook? {
    return _getClassPlaygroundQuickLook(data)
  }
  public var disposition: MirrorDisposition { return .ObjCObject }
}
#endif
