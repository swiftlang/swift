// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100 -solver-enable-prune-disjunctions

// REQUIRES: objc_interop

// This is valid, but slow.

import Foundation

func f(str: CFString!) {
  let _ = [
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str)
  ]
}
