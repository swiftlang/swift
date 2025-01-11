//===--- StringFamilyExtensions.swift -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension String.UTF8View {

  @available(SwiftStdlib 6.1, *)
  public var storage: Span<UTF8.CodeUnit> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let count = _guts.count
      if _guts.isSmall {
        let a = Builtin.addressOfBorrow(self)
        let address = UnsafePointer<UTF8.CodeUnit>(a)
        let span = Span(_unsafeStart: address, count: count)
        return _overrideLifetime(span, borrowing: self)
      }
      else if _guts.isFastUTF8 {
        let buffer = _guts._object.fastUTF8
        _internalInvariant(count == buffer.count)
        let span = Span(_unsafeElements: buffer)
        return _overrideLifetime(span, borrowing: self)
      }
      // handle other Objective-C bridging cases here
      fatalError("Some bridged Strings are not supported at this time")
    }
  }
}

extension Substring.UTF8View {

  @available(SwiftStdlib 6.1, *)
  public var storage: Span<UTF8.CodeUnit> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let start = _slice._startIndex._encodedOffset
      let end = _slice._endIndex._encodedOffset
      if _wholeGuts.isSmall {
        let a = Builtin.addressOfBorrow(self)
        let offset = start &+ (MemoryLayout<String.Index>.stride &<< 1)
        let address = UnsafePointer<UTF8.CodeUnit>(a).advanced(by: offset)
        let span = Span(_unsafeStart: address, count: end &- start)
        return _overrideLifetime(span, borrowing: self)
      }
      else if _wholeGuts.isFastUTF8 {
        let buffer = _wholeGuts._object.fastUTF8.extracting(start..<end)
        let count = end &- start
        _internalInvariant(count == buffer.count)
        let span = Span(_unsafeElements: buffer)
        return _overrideLifetime(span, borrowing: self)
      }
      // handle other Objective-C bridging cases here
      fatalError("Some bridged Strings are not supported at this time")
    }
  }
}
