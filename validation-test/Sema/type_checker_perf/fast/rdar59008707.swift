// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000

// REQUIRES: objc_interop

import Foundation
import Combine

func slow() {
  let nums: [Double] = []

  let publisher = Publishers.Sequence<[Double], Never>(sequence: nums)
  let _ = publisher.map { sqrt($0) }
                   .filter { $0 < 0 }
                   .map { $0 * 0 + 1.0 }
                   .map { "\($0)" }
                   .sink { print($0) }
}
