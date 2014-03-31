//===--- ArrayType.swift - Protocol for Array-like types ------------------===//
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

protocol ArrayType
  // FIXME: must start with Collection; see <rdar://problem/16465340> 
  : Collection, 
    _ArrayType,
    MutableCollection, 
    Sliceable,
    ArrayLiteralConvertible,
    ConditionallyBridgedToObjectiveC
{
  //===--- public interface -----------------------------------------------===//
  init()
  
  var count: Int {get}
  var capacity: Int {get}
  
  mutating func reserve(n: Int)
  mutating func append(element: Self.GeneratorType.Element)

  //===--- implementation detail  -----------------------------------------===//

  typealias Buffer : ArrayBufferType
  init(_: Buffer)
  
  mutating func _adopt(
    newBuffer: NativeArrayBuffer<Self.GeneratorType.Element>)
  
  // Returns true iff Self can allow its buffer to be directly
  // mutated.  *Must* remain a mutating function and not a get-only
  // property if we are to have a hope of accurate uniqueness checks
  mutating func _hasMutableBuffer() -> Bool

  // Copy elements from the given subRange into the range starting at
  // target, returning a pointer past-the-end of the target range
  func _uninitializedCopy(
    subRange: Range<Self.IndexType>,
    target: UnsafePointer<Self.GeneratorType.Element>
  ) -> UnsafePointer<Self.GeneratorType.Element>

  // Update this Array's idea of its count.  Does NOT construct or
  // destroy elements to ensure that count is accurate.
  mutating func _updateCount(newCount: Int)
  
  var buffer: Buffer {get set}
}
