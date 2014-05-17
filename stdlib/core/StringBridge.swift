//===----------------------------------------------------------------------===//
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

// Swift's String bridges NSString via this protocol and these
// variables, allowing the core stdlib to remain decoupled from
// Foundation.

/// Effectively a proxy for NSString that doesn't mention it by
/// name.  NSString's conformance to this protocol is declared in
/// Foundation.
@class_protocol @objc protocol _CocoaString {}

/// Loading Foundation initializes these function variables
/// with useful values

/// Produces a `_StringBuffer` from a given subrange of a source
/// `_CocoaString`, having the given minimum capacity.
var _cocoaStringToContiguous: (
  source: _CocoaString, range: Range<Int>, minimumCapacity: Int
) -> _StringBuffer = _cocoaStringToContiguousNotInitialized

func _cocoaStringToContiguousNotInitialized(
  source: _CocoaString, range: Range<Int>, minimumCapacity: Int
) -> _StringBuffer {
  _fatalError("_cocoaStringToContiguous not initialized")
}

/// Reads the entire contents of a _CocoaString into contiguous
/// storage of sufficient capacity.
var _cocoaStringReadAll: (
  source: _CocoaString, destination: UnsafePointer<UTF16.CodeUnit>
) -> Void = _cocoaStringReadAllNotInitialized

func _cocoaStringReadAllNotInitialized(
  source: _CocoaString, destination: UnsafePointer<UTF16.CodeUnit>
) -> Void {
  _fatalError("_cocoaStringReadAll not initialized")
}

var _cocoaStringLength: (
  source: _CocoaString
) -> Int = _cocoaStringLengthNotInitialized

func _cocoaStringLengthNotInitialized(
  source: _CocoaString
) -> Int {
  _fatalError("_cocoaStringLength not initialized")
}

var _cocoaStringSlice: (
  target: _StringCore, subRange: Range<Int>
) -> _StringCore = _cocoaStringSliceNotInitialized

func _cocoaStringSliceNotInitialized(
  target: _StringCore, subRange: Range<Int>
) -> _StringCore {
  _fatalError("_cocoaStringSlice not initialized")
}

var _cocoaStringSubscript: (
  target: _StringCore, position: Int
) -> UTF16.CodeUnit = _cocoaStringSubscriptNotInitialized

func _cocoaStringSubscriptNotInitialized(
  target: _StringCore, position: Int
) -> UTF16.CodeUnit {
  _fatalError("_cocoaStringSubscript not initialized")
}

var _cocoaStringEncodeSomeUTF8: (
  target: _StringCore, position: Int
) -> (_StringCore.IndexType, _StringCore.UTF8Chunk)
  = _cocoaStringEncodeSomeUTF8NotInitialized

func _cocoaStringEncodeSomeUTF8NotInitialized(
  target: _StringCore, position: Int 
) -> (_StringCore.IndexType, _StringCore.UTF8Chunk) {
  _fatalError("_cocoaStringEncodeSomeUTF8 not initialized")
}

