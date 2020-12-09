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
  public func contains(where predicate: (Element) async throws -> Bool) async throws /*rethrows*/ -> Bool {
    var it = makeAsyncIterator()
    while let e = await try it.next() {
      if await try predicate(e) {
        return true
      }
    }
    return false
  }
}

extension AsyncSequence where Element: Equatable {
  public func contains(_ element: Element) async throws /*rethrows*/ -> Bool {
    return await try contains { $0 == element }
  }
}
