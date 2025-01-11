//===--- ArrayTypeExtensions.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension ContiguousArray {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Element> {
    get {
      let (pointer, count) = (_buffer.firstElementAddress, _buffer.count)
      let span = Span(_unsafeStart: pointer, count: count)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension ArraySlice {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Element> {
    get {
      let (pointer, count) = (_buffer.firstElementAddress, _buffer.count)
      let span = Span(_unsafeStart: pointer, count: count)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

#if _runtime(_ObjC)
import SwiftShims
#endif

extension Array {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Element> {
    get {
#if _runtime(_ObjC)
      if _slowPath(!_buffer._isNative) {
        let buffer = _buffer.getOrAllocateAssociatedObjectBuffer()
        let (pointer, count) = (buffer.firstElementAddress, buffer.count)
        let span = Span(_unsafeStart: pointer, count: count)
        return _overrideLifetime(span, borrowing: self)
      }
#endif
      let (pointer, count) = (_buffer.firstElementAddress, _buffer.count)
      let span = Span(_unsafeStart: pointer, count: count)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}
