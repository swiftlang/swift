//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import Builtin

@usableFromInline
internal func unsafeBitCast<T: ~Escapable & ~Copyable, U>(
   _ x: consuming T, to type: U.Type
) -> U {
  Builtin.reinterpretCast(x)
}

/// Unsafely discard any lifetime dependency on the `dependent` argument. Return
/// a value identical to `dependent` with a lifetime dependency on the caller's
/// borrow scope of the `source` argument.
///
/// This mimics the stdlib definition. It is public for use with import macros.
@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(borrow source)
public func _cxxOverrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, borrowing source: borrowing U
) -> T {
  // TODO: Remove @_unsafeNonescapableResult. Instead, the unsafe dependence
  // should be expressed by a builtin that is hidden within the function body.
  dependent
}

/// Unsafely discard any lifetime dependency on the `dependent` argument. Return
/// a value identical to `dependent` that inherits all lifetime dependencies from
/// the `source` argument.
///
/// This mimics the stdlib definition. It is public for use with import macros.
@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(copy source)
public func _cxxOverrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, copying source: borrowing U
) -> T {
  // TODO: Remove @_unsafeNonescapableResult. Instead, the unsafe dependence
  // should be expressed by a builtin that is hidden within the function body.
  dependent
}

/// A C++ type that is an object that can refer to a contiguous sequence of objects.
///
/// C++ standard library type `std::span` conforms to this protocol.
public protocol CxxSpan<Element> {
  associatedtype Element
  associatedtype Size: BinaryInteger

  init()
  init(_ unsafePointer: UnsafePointer<Element>!, _ count: Size)

  func size() -> Size
  func __dataUnsafe() -> UnsafePointer<Element>?
}

extension CxxSpan {
  /// Creates a C++ span from a Swift UnsafeBufferPointer
  @inlinable
  public init(_ unsafeBufferPointer: UnsafeBufferPointer<Element>) {
    unsafe precondition(unsafeBufferPointer.baseAddress != nil, 
                  "UnsafeBufferPointer should not point to nil")
    unsafe self.init(unsafeBufferPointer.baseAddress!, Size(unsafeBufferPointer.count))
  }

  @inlinable
  public init(_ unsafeMutableBufferPointer: UnsafeMutableBufferPointer<Element>) {
    unsafe precondition(unsafeMutableBufferPointer.baseAddress != nil, 
                  "UnsafeMutableBufferPointer should not point to nil")
    unsafe self.init(unsafeMutableBufferPointer.baseAddress!, Size(unsafeMutableBufferPointer.count))
  }

  @available(SwiftCompatibilitySpan 5.0, *)
  @inlinable
  @unsafe
  public init(_ span: Span<Element>) {
    let (p, c) = unsafe unsafeBitCast(span, to: (UnsafeRawPointer?, Int).self)
    unsafe precondition(p != nil, "Span should not point to nil")
    let binding = unsafe p!.bindMemory(to: Element.self, capacity: c)
    unsafe self.init(binding, Size(c))
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
extension Span {
  @_alwaysEmitIntoClient
  @unsafe
  @_unsafeNonescapableResult
  @lifetime(borrow span)
  public init<T: CxxSpan<Element>>(
    _unsafeCxxSpan span: borrowing T,
  ) {
    let buffer = unsafe UnsafeBufferPointer(start: span.__dataUnsafe(), count: Int(span.size()))
    let newSpan = unsafe Span(_unsafeElements: buffer)
    // 'self' is limited to the caller's scope of the variable passed to the 'span' argument.
    self = unsafe _cxxOverrideLifetime(newSpan, borrowing: span)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
extension MutableSpan {
  @_alwaysEmitIntoClient
  @unsafe
  @_unsafeNonescapableResult
  @lifetime(borrow span)
  public init<T: CxxMutableSpan<Element>>(
    _unsafeCxxSpan span: borrowing T,
  ) {
    let buffer = unsafe UnsafeMutableBufferPointer(start: span.__dataUnsafe(), count: Int(span.size()))
    let newSpan = unsafe MutableSpan(_unsafeElements: buffer)
    // 'self' is limited to the caller's scope of the variable passed to the 'span' argument.
    self = unsafe _cxxOverrideLifetime(newSpan, borrowing: span)
  }
}

public protocol CxxMutableSpan<Element> {
  associatedtype Element
  associatedtype Size: BinaryInteger

  init()
  init(_ unsafeMutablePointer: UnsafeMutablePointer<Element>!, _ count: Size)

  func size() -> Size
  func __dataUnsafe() -> UnsafeMutablePointer<Element>?
}

extension CxxMutableSpan {
  /// Creates a C++ span from a Swift UnsafeMutableBufferPointer
  @inlinable
  public init(_ unsafeMutableBufferPointer: UnsafeMutableBufferPointer<Element>) {
    unsafe precondition(unsafeMutableBufferPointer.baseAddress != nil, 
                  "UnsafeMutableBufferPointer should not point to nil")
    unsafe self.init(unsafeMutableBufferPointer.baseAddress!, Size(unsafeMutableBufferPointer.count))
  }

  @available(SwiftCompatibilitySpan 5.0, *)
  @inlinable
  @unsafe
  public init(_ span: consuming MutableSpan<Element>) {
    let (p, c) = unsafe unsafeBitCast(span, to: (UnsafeMutableRawPointer?, Int).self)
    unsafe precondition(p != nil, "Span should not point to nil")
    let binding = unsafe p!.bindMemory(to: Element.self, capacity: c)
    unsafe self.init(binding, Size(c))
  }
}
