//===--- ObjCMirrors.swift ------------------------------------------------===//
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

#if _runtime(_ObjC)
@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
@_silgen_name("swift_ObjCMirror_count") 
internal func _getObjCCount(_: _MagicMirrorData) -> Int
@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
@_silgen_name("swift_ObjCMirror_subscript") 
internal func _getObjCChild<T>(_: Int, _: _MagicMirrorData) -> (T, _Mirror)

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal func _getObjCSummary(_ data: _MagicMirrorData) -> String {
  let theDescription = _swift_stdlib_objcDebugDescription(data._loadValue(ofType: AnyObject.self)) as AnyObject
  return _cocoaStringToSwiftString_NonASCII(theDescription)
}

public // SPI(runtime)
struct _ObjCMirror : _Mirror {
  @_versioned // FIXME(sil-serialize-all)
  internal let data: _MagicMirrorData

  @_inlineable // FIXME(sil-serialize-all)
  public var value: Any { return data.objcValue }
  @_inlineable // FIXME(sil-serialize-all)
  public var valueType: Any.Type { return data.objcValueType }
  @_inlineable // FIXME(sil-serialize-all)
  public var objectIdentifier: ObjectIdentifier? {
    return data._loadValue(ofType: ObjectIdentifier.self)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var count: Int {
    return _getObjCCount(data)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public subscript(i: Int) -> (String, _Mirror) {
    return _getObjCChild(i, data)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var summary: String {
    return _getObjCSummary(data)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var quickLookObject: PlaygroundQuickLook? {
    let object = _swift_ClassMirror_quickLookObject(data)
    return _getClassPlaygroundQuickLook(object)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var disposition: _MirrorDisposition { return .objCObject }
}

public // SPI(runtime)
struct _ObjCSuperMirror : _Mirror {
  @_versioned // FIXME(sil-serialize-all)
  internal let data: _MagicMirrorData

  @_inlineable // FIXME(sil-serialize-all)
  public var value: Any { return data.objcValue }
  @_inlineable // FIXME(sil-serialize-all)
  public var valueType: Any.Type { return data.objcValueType }

  // Suppress the value identifier for super mirrors.
  @_inlineable // FIXME(sil-serialize-all)
  public var objectIdentifier: ObjectIdentifier? {
    return nil
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var count: Int {
    return _getObjCCount(data)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public subscript(i: Int) -> (String, _Mirror) {
    return _getObjCChild(i, data)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var summary: String {
    return _getObjCSummary(data)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var quickLookObject: PlaygroundQuickLook? {
    let object = _swift_ClassMirror_quickLookObject(data)
    return _getClassPlaygroundQuickLook(object)
  }
  @_inlineable // FIXME(sil-serialize-all)
  public var disposition: _MirrorDisposition { return .objCObject }
}
#endif
