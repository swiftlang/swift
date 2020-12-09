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
  public func min(by areInIncreasingOrder: (Element, Element) async throws -> Bool) async throws /*rethrows*/ -> Element? {
    var it = makeAsyncIterator()
    guard var result = await try it.next() else { return nil }
    while let e = await try it.next() {
      if await try areInIncreasingOrder(e, result) { result = e }
    }
    return result
  }
  
  public func max(by areInIncreasingOrder: (Element, Element) async throws -> Bool) async throws /*rethrows*/ -> Element? {
    var it = makeAsyncIterator()
    guard var result = await try it.next() else { return nil }
    while let e = await try it.next() {
      if await try areInIncreasingOrder(result, e) { result = e }
    }
    return result
  }
}

extension AsyncSequence where Element: Comparable {
  public func min() async throws /*rethrows*/ -> Element? {
    return await try min(by: <)
  }
  
  public func max() async throws /*rethrows*/ -> Element? {
    return await try max(by: <)
  }
}
