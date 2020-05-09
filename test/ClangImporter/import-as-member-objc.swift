// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/../IDE/Inputs/custom-modules %s -verify -enable-objc-interop

import ImportAsMember.Class

func doIt(s: SomeClass) {
  s.doIt()
}

// Make sure we can't find doIt() via dynamic lookup.
func doItDynamic(s: AnyObject) {
  s.doIt() // expected-error {{value of type 'AnyObject' has no member 'doIt'}}
}
