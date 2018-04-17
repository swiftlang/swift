// REQUIRES: objc_interop
// REQUIRES: OS=macosx
//
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers %s -verify

import NamedLazyMembers

func callViaAnyObject(ao: AnyObject, d: Double) {
  ao.doSomething(d, celsius: d)
}

func callDirect(snt: SimilarlyNamedThings, d: Double) {
  snt.doSomething(d, fahrenheit: d) {
  }
}
