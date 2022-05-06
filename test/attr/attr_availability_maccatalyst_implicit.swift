// RUN: %target-swift-frontend -I %t -I %S/Inputs/custom-modules -parse-stdlib -parse-as-library -typecheck -verify -target x86_64-apple-ios15.4-macabi %s

// REQUIRES: objc_interop
// REQUIRES: maccatalyst_support

import Available_NSObject

@available(iOS 15.0, *)
open class OverAvailableClass: NSBaseClass {}
// expected-error@-1 {{initializer cannot be more available than enclosing scope}}
// expected-note@-2 {{initializer implicitly declared here with availability of Mac Catalyst 13.1 or newer}}
// expected-note@-3 {{enclosing scope requires availability of Mac Catalyst 15.0 or newer}}

