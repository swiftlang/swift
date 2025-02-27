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
internal func unsafeBitCast<T: ~Escapable, U>(
   _ x: T, to type: U.Type
) -> U {
  Builtin.reinterpretCast(x)
}

/// Used by SwiftifyImport to work around a compiler diagnostic. It should be removed once the
/// workaround is no longer needed.
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
public func _unsafeRemoveLifetime<T: ~Copyable & ~Escapable>(_ dependent: consuming T) -> T {
  dependent
}

/// Unsafely discard any lifetime dependency on the `dependent` argument. Return
/// a value identical to `dependent` with a lifetime dependency on the caller's
/// borrow scope of the `source` argument.
@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(borrow source)
internal func _overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, borrowing source: borrowing U
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
  init(_ unsafePointer : UnsafePointer<Element>, _ count: Size)

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

  @available(SwiftStdlib 6.1, *)
  @inlinable
  @unsafe
  public init(_ span: Span<Element>) {
    let (p, c) = unsafe unsafeBitCast(span, to: (UnsafeRawPointer?, Int).self)
    unsafe precondition(p != nil, "Span should not point to nil")
    let binding = unsafe p!.bindMemory(to: Element.self, capacity: c)
    unsafe self.init(binding, Size(c))
  }
}

@available(SwiftStdlib 6.1, *)
extension Span {
  @_alwaysEmitIntoClient
  @unsafe
  @_unsafeNonescapableResult
  public init<T: CxxSpan<Element>>(
    _unsafeCxxSpan span: borrowing T,
  ) {
    let buffer = unsafe UnsafeBufferPointer(start: span.__dataUnsafe(), count: Int(span.size()))
    let newSpan = Span(_unsafeElements: buffer)
    // 'self' is limited to the caller's scope of the variable passed to the 'span' argument.
    self = unsafe _overrideLifetime(newSpan, borrowing: span)
  }
}

public protocol CxxMutableSpan<Element> {
  associatedtype Element
  associatedtype Size: BinaryInteger

  init()
  init(_ unsafeMutablePointer : UnsafeMutablePointer<Element>, _ count: Size)
}

extension CxxMutableSpan {
  /// Creates a C++ span from a Swift UnsafeMutableBufferPointer
  @inlinable
  public init(_ unsafeMutableBufferPointer: UnsafeMutableBufferPointer<Element>) {
    unsafe precondition(unsafeMutableBufferPointer.baseAddress != nil, 
                  "UnsafeMutableBufferPointer should not point to nil")
    unsafe self.init(unsafeMutableBufferPointer.baseAddress!, Size(unsafeMutableBufferPointer.count))
  }
}
