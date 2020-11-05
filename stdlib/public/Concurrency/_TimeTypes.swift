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
@_implementationOnly import _SwiftConcurrencyShims

// FIXME: This file and all "time types" defined here are temporary until we decide what types to use.
//        It was suggested to avoid Dispatch types in public API of Swift Concurrency, 
//        so for now we use "bare minimum" types just to be able to continue prototyping.
extension Task {
  /// Represents a time interval, i.e. a number of seconds.
  ///
  /// It can be used to express deadlines, in the form of time interval from "now."
  ///
  /// - Note: This is equivalent to `DispatchTimeInterval` if we were to use it.
  public struct _TimeInterval: Equatable, Comparable {
    let nanoseconds: UInt64

    private init(nanoseconds: UInt64) {
      self.nanoseconds = nanoseconds
    }

    public static func seconds(_ s: UInt64) -> Self {
      .init(nanoseconds: clampedInt64Product(s, 1_000_000_000))
    }

    public static func milliseconds(_ ms: UInt64) -> Self {
      .init(nanoseconds: clampedInt64Product(ms, 1_000_000))
    }

    public static func microseconds(_ us: UInt64) -> Self {
      .init(nanoseconds: clampedInt64Product(us, 1_000))
    }

    public static func nanoseconds(_ ns: UInt64) -> Self {
      .init(nanoseconds: ns)
    }

    public static var never: Self {
      .init(nanoseconds: .max)
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
      lhs.nanoseconds < rhs.nanoseconds
    }
  }

}

// Returns m1 * m2, clamped to the range [UInt64.min, UInt64.max].
private func clampedInt64Product(_ m1: UInt64, _ m2: UInt64) -> UInt64 {
  let (result, overflow) = m1.multipliedReportingOverflow(by: m2)
  if overflow {
    return UInt64.max
  }
  return result
}
