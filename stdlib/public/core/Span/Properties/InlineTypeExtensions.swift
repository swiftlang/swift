//===--- InlineTypeExtensions.swift ---------------------------------------===//
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

extension CollectionOfOne {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Element> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Element>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 1)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension SIMD2 {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Scalar> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Scalar>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 2)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension SIMD3 {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Scalar> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Scalar>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 3)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension SIMD4 {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Scalar> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Scalar>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 4)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension SIMD8 {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Scalar> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Scalar>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 8)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension SIMD16 {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Scalar> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Scalar>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 16)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension SIMD32 {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Scalar> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Scalar>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 32)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

extension SIMD64 {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Scalar> {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      let pointer = UnsafePointer<Scalar>(Builtin.addressOfBorrow(self))
      let span = Span(_unsafeStart: pointer, count: 64)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension Slab where Element: ~Copyable {

  @available(SwiftStdlib 6.2, *)
  public var storage: Span<Element> {
    @_alwaysEmitIntoClient
    @_addressableSelf
    @lifetime(borrow self)
    borrowing get {
      let pointer = _address
      let span = Span(_unsafeStart: pointer, count: count)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}
