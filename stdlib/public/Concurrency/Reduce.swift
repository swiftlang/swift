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
  public func reduce<Result>(_ initialResult: Result, _ nextPartialResult: (_ partialResult: Result, Element) async throws -> Result) async rethrows -> Result {
    var accumulator = initialResult
    var it = makeAsyncIterator()
    while let element = try await it.next() {
      accumulator = try await nextPartialResult(accumulator, element)
    }
    return accumulator
  }
  
  public func reduce<Result>(into initialResult: __owned Result, _ updateAccumulatingResult: (_ partialResult: inout Result, Element) async throws -> Void) async rethrows -> Result {
    var accumulator = initialResult
    var it = makeAsyncIterator()
    while let element = try await it.next() {
      try await updateAccumulatingResult(&accumulator, element)
    }
    return accumulator
  }

}
