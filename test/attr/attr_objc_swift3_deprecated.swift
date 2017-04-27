// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 3 -warn-swift3-objc-inference
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { } // expected-warning{{inference of '@objc' for members of Objective-C-derived classes is deprecated}}
    // expected-note@-1{{add `@objc` to continue exposing an Objective-C entry point (Swift 3 behavior)}}{{3-3=@objc }}
    // expected-note@-2{{add `@nonobjc` to suppress the Objective-C entry point (Swift 4 behavior)}}{{3-3=@nonobjc }}

  var bar: NSObject? = nil // expected-warning{{inference of '@objc' for members of Objective-C-derived classes is deprecated}}
    // expected-note@-1{{add `@objc` to continue exposing an Objective-C entry point (Swift 3 behavior)}}{{3-3=@objc }}
    // expected-note@-2{{add `@nonobjc` to suppress the Objective-C entry point (Swift 4 behavior)}}{{3-3=@nonobjc }}
}

class DynamicMembers {
  dynamic func foo() { } // expected-warning{{inference of '@objc' for 'dynamic' members is deprecated}}{{3-3=@objc }}
  
  dynamic var bar: NSObject? = nil // expected-warning{{inference of '@objc' for 'dynamic' members is deprecated}}{{3-3=@objc }}
}

// Suppress diagnostices about references to inferred @objc declarations
// in this mode.
func test(sc: ObjCSubclass, dm: DynamicMembers) {
  _ = #selector(sc.foo)
  _ = #selector(getter: dm.bar)
  _ = #keyPath(DynamicMembers.bar)
}
