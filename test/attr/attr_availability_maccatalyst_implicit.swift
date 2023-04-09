// RUN: %target-swift-frontend -I %t -I %S/Inputs/custom-modules -parse-stdlib -parse-as-library -typecheck -verify -target %target-cpu-apple-ios15.4-macabi %s

// REQUIRES: objc_interop
// REQUIRES: maccatalyst_support

import Available_NSObject

@available(iOS 15.0, *)
open class OverAvailableClass: NSBaseClass {}
// expected-warning@-1 {{initializer cannot be more available than enclosing scope}}
// expected-note@-2 {{initializer implicitly declared here with availability of Mac Catalyst 13.1 or newer}}
// expected-note@-3 {{enclosing scope requires availability of Mac Catalyst 15.0 or newer}}

extension NSBaseClass {
  @available(iOS 15.0, *)
  // expected-warning@+3 {{_modify accessor cannot be more available than enclosing scope}}
  // expected-note@+2 {{_modify accessor implicitly declared here with availability of Mac Catalyst 13.1 or newer}}
  // expected-note@+1 {{enclosing scope requires availability of Mac Catalyst 15.0 or newer}}
  var property: Int {
    get { 1 }
    set {}
  }
}
