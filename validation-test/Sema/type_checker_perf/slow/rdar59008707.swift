// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000 -solver-enable-prune-disjunctions

// FIXME: -solver-enable-prune-disjunctions just moves the location of the
// reasonable time diagnostic. It also decreases the number of scopes but
// not below 10000.

// REQUIRES: objc_interop

// This type checks successfully with default limits, but slow (600k scopes).

import Foundation
import Combine

func slow() {
  var nums: [Double] = []

  let publisher = Publishers.Sequence<[Double], Never>(sequence: nums)
  let _ = publisher.map({sqrt($0)}).filter({$0 < 0}).map({$0 * 0 + 1.0}).map({"\($0)"}).sink {print($0)}
  // expected-error@-1 {{reasonable time}}
}

