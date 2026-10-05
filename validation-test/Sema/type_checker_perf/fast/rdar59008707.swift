// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000 -solver-enable-promote-supertypes
// RUN: %target-typecheck-verify-swift -solver-scope-threshold=80000 -solver-disable-promote-supertypes

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

