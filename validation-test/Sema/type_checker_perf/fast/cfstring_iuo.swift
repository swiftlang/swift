// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000 -solver-enable-prune-disjunctions

// REQUIRES: objc_interop

import Foundation

func f(str: CFString!) {
  let _ = [
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str)
  ]
}