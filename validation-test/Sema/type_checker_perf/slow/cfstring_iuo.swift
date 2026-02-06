// RUN: %target-typecheck-verify-swift -solver-scope-threshold=8000

// REQUIRES: objc_interop

import Foundation

func f(str: CFString!) {
  let _ = [ // expected-error {{reasonable time}}
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str),
    str: String(str)
  ]
}