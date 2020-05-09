// RUN: %target-swift-frontend -sil-verify-all -emit-sil %s -o /dev/null -verify
// REQUIRES: objc_interop

import Foundation

@_transparent public func a(_ condition: @autoclosure () -> Bool) {
  _ = condition()
}

func callsA() {
  let x = (2 as NSNumber) as AnyObject
  a(x.isEqual(2))
}
