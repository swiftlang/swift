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
@class_protocol @objc public protocol _CocoaStringType {}

/// Loading Foundation initializes these function variables
/// with useful values

/// Produces a `_StringBuffer` from a given subrange of a source
/// `_CocoaStringType`, having the given minimum capacity.
public var _cocoaStringToContiguous: (
  source: _CocoaStringType, range: Range<Int>, minimumCapacity: Int
) -> _StringBuffer = _cocoaStringToContiguousNotInitialized

func _cocoaStringToContiguousNotInitialized(
  source: _CocoaStringType, range: Range<Int>, minimumCapacity: Int
) -> _StringBuffer {
  _fatalError("_cocoaStringToContiguous not initialized")
}

/// Reads the entire contents of a _CocoaStringType into contiguous
/// storage of sufficient capacity.
public var _cocoaStringReadAll: (
  source: _CocoaStringType, destination: UnsafePointer<UTF16.CodeUnit>
) -> Void = _cocoaStringReadAllNotInitialized

func _cocoaStringReadAllNotInitialized(
  source: _CocoaStringType, destination: UnsafePointer<UTF16.CodeUnit>
) -> Void {
  _fatalError("_cocoaStringReadAll not initialized")
}

public var _cocoaStringLength: (
  source: _CocoaStringType
) -> Int = _cocoaStringLengthNotInitialized

func _cocoaStringLengthNotInitialized(
  source: _CocoaStringType
) -> Int {
  _fatalError("_cocoaStringLength not initialized")
}

public var _cocoaStringSlice: (
  target: _StringCore, subRange: Range<Int>
) -> _StringCore = _cocoaStringSliceNotInitialized

func _cocoaStringSliceNotInitialized(
  target: _StringCore, subRange: Range<Int>
) -> _StringCore {
  _fatalError("_cocoaStringSlice not initialized")
}

public var _cocoaStringSubscript: (
  target: _StringCore, position: Int
) -> UTF16.CodeUnit = _cocoaStringSubscriptNotInitialized

func _cocoaStringSubscriptNotInitialized(
  target: _StringCore, position: Int
) -> UTF16.CodeUnit {
  _fatalError("_cocoaStringSubscript not initialized")
}

