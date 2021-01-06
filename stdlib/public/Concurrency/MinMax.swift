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

extension AsyncSequence {
  public func min(by areInIncreasingOrder: (Element, Element) async throws -> Bool) async rethrows -> Element? {
    var it = makeAsyncIterator()
    guard var result = try await it.next() else { return nil }
    while let e = try await it.next() {
      if try await areInIncreasingOrder(e, result) { result = e }
    }
    return result
  }
  
  public func max(by areInIncreasingOrder: (Element, Element) async throws -> Bool) async rethrows -> Element? {
    var it = makeAsyncIterator()
    guard var result = try await it.next() else { return nil }
    while let e = try await it.next() {
      if try await areInIncreasingOrder(result, e) { result = e }
    }
    return result
  }
}

extension AsyncSequence where Element: Comparable {
  public func min() async rethrows -> Element? {
    return try await min(by: <)
  }
  
  public func max() async rethrows -> Element? {
    return try await max(by: <)
  }
}
