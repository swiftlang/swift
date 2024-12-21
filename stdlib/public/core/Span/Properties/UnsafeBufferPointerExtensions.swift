//===--- UnsafeBufferPointerExtensions.swift ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension UnsafeBufferPointer {

  @available(SwiftStdlib 6.1, *)
  public var storage: Span<Element> {
    @lifetime(borrow self)
    get {
      let span = Span(_unsafeElements: self)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension UnsafeMutableBufferPointer {

  @available(SwiftStdlib 6.1, *)
  public var storage: Span<Element> {
    @lifetime(borrow self)
    get {
      let span = Span(_unsafeElements: self)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension UnsafeRawBufferPointer {

  @available(SwiftStdlib 6.1, *)
  public var bytes: RawSpan {
    @lifetime(borrow self)
    get {
      let span = RawSpan(_unsafeBytes: self)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension UnsafeMutableRawBufferPointer {

  @available(SwiftStdlib 6.1, *)
  public var bytes: RawSpan {
    @lifetime(borrow self)
    get {
      let span = RawSpan(_unsafeBytes: self)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}
