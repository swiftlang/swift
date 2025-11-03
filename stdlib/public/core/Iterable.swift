//===----------------------------------------------------------------------===//
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

@available(SwiftStdlib 6.3, *)
@frozen
public enum EstimatedCount {
  case infinite
  case exactly(Int)
  case unknown
}

@available(SwiftStdlib 6.3, *)
public protocol Iterable<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable
  associatedtype BorrowIterator: BorrowIteratorProtocol<Element> & ~Copyable & ~Escapable

  var isEmpty: Bool { get }

  var estimatedCount: EstimatedCount { get }

  @_lifetime(borrow self)
  borrowing func startBorrowIteration() -> BorrowIterator

  func _customContainsEquatableElement(
    _ element: borrowing Element
  ) -> Bool?
}

@available(SwiftStdlib 6.3, *)
extension Iterable where Self: ~Copyable & ~Escapable {
  @_alwaysEmitIntoClient
  @_transparent
  public var underestimatedCount: Int {
    switch estimatedCount {
    case .infinite:
      .max
    case .exactly(let c):
      c
    case .unknown:
      0
    }
  }

  @_alwaysEmitIntoClient
  @_transparent
  public func _customContainsEquatableElement(_ element: borrowing Element) -> Bool? {
    nil
  }
}
