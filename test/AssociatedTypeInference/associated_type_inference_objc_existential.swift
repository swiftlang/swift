// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

protocol P1 {
  associatedtype A: NSObject
}

@objc protocol P2 {}

struct S: P1 {
  typealias A = NSObject & P2
}

let x: (any NSObject & P2).Type = S.A.self
