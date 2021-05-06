// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

class X: NSObject {
  // expected-error@+1 {{'async' initializer cannot be represented in Objective-C}}
  @objc init(_ i : Int) async { }
}

