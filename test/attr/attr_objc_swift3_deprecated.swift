// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 3 -warn-swift3-objc-inference
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { } // expected-warning{{inference of '@objc' for members of Objective-C--derived classes is deprecated in Swift 4}}{{3-3=@objc }}
  var bar: NSObject? = nil // expected-warning{{inference of '@objc' for members of Objective-C--derived classes is deprecated in Swift 4}}{{3-3=@objc }}
}

class DynamicMembers {
  dynamic func foo() { } // expected-warning{{inference of '@objc' for 'dynamic' members is deprecated in Swift 4}}{{3-3=@objc }}
  
  dynamic var bar: NSObject? = nil // expected-warning{{inference of '@objc' for 'dynamic' members is deprecated in Swift 4}}{{3-3=@objc }}
}

