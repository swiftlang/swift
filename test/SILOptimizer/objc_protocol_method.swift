// RUN: %target-swift-frontend -emit-sil %s
// REQUIRES: objc_interop

// Just make sure this doesn't crash.

import Foundation

@objc protocol P {
  func f()
}

class C : P {
  func f() {}
}

@_transparent func g<T : P>(_ t: T) {
  t.f()
}

func callsG(_ c: C) {
  g(c)
}

callsG(C())
