// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

final class A: NSObject, ConcurrentValue {
  let x: Int = 5
}

final class B: NSObject, ConcurrentValue {
  var x: Int = 5 // expected-error{{stored property 'x' of 'ConcurrentValue'-conforming class 'B' is mutable}}
}

class C { }

final class D: NSObject, ConcurrentValue {
  let c: C = C() // expected-error{{stored property 'c' of 'ConcurrentValue'-conforming class 'D' has non-concurrent-value type 'C'}}
}


