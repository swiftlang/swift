// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4
// REQUIRES: objc_interop

import ObjectiveC
import Foundation

class X: NSObjectProtocol { } // expected-warning{{'NSObjectProtocol' is deprecated: all classes implicitly conform to the 'NSObject' protocol}}
// expected-note@-1{{use 'AnyObject' instead}}
// expected-warning@-2{{conformance of class 'X' to 'AnyObject' is redundant}}

protocol P: NSObjectProtocol { } // expected-warning{{'NSObjectProtocol' is deprecated: all classes implicitly conform to the 'NSObject' protocol}}
// expected-note@-1{{use 'AnyObject' instead}}

func composition(_: NSCoding & NSObjectProtocol) { } // expected-warning{{'NSObjectProtocol' is deprecated: all classes implicitly conform to the 'NSObject' protocol}}
// expected-note@-1{{use 'AnyObject' instead}}

