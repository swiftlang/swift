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

/// A C++ type that represents a stack.
///
/// C++ standard library types `std::stack` conforms to this protocol.
public protocol CxxStack<Element> {
  associatedtype Element
  associatedtype Size: BinaryInteger
  associatedtype RawReference: UnsafeCxxInputIterator
    where RawReference.Pointee == Element

  func empty() -> Bool
  func size() -> Size

  /// Do not implement this function manually in Swift.
  func __topUnsafe() -> RawReference

  /// Do not implement this function manually in Swift.
  mutating func __popUnsafe()

  mutating func push(_ element: Element)
}

public extension CxxStack {
  @inlinable var count: Int { Int(self.size()) }
  @inlinable var isEmpty: Bool { self.empty() }

  @inlinable
  func top() -> Element? {
    if self.empty() {
      return nil
    }
    return self.__topUnsafe().pointee
  }

  @inlinable
  @discardableResult
  mutating func pop() -> Element? {
    if self.empty() {
      return nil
    }
    let result: Element = self.__topUnsafe().pointee
    self.__popUnsafe()
    return result
  }
}
