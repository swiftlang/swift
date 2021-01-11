////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2020 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

import Swift

@rethrows
public protocol AsyncIteratorProtocol {
  associatedtype Element
  mutating func next() async throws -> Element?
  mutating func cancel()
}

internal enum _OptionalAsyncIterator<AsyncIterator: AsyncIteratorProtocol>: AsyncIteratorProtocol {
  case none
  case some(AsyncIterator)

  init(_ iterator: AsyncIterator) {
    self = .some(iterator)
  }

  mutating func next() async rethrows -> AsyncIterator.Element? {
    switch self {
    case .some(var iterator):
      defer { self = .some(iterator) }
      return try await iterator.next()
    default:
      return nil
    }
  }

  mutating func cancel() {
    switch self {
    case .some(var iterator):
      iterator.cancel()
      self = .none
    default:
      break
    }
  }
}