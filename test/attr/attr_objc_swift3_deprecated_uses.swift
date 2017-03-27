// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 3
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { } // expected-note 2{{add '@objc' to expose this instance method to Objective-C}}{{3-3=@objc }}
  var bar: NSObject? = nil // expected-note{{add '@objc' to expose this var to Objective-C}}{{3-3=@objc }}
}

class DynamicMembers {
  dynamic func foo() { }
  
  dynamic var bar: NSObject? = nil // expected-note 2{{add '@objc' to expose this var to Objective-C}}{{3-3=@objc }}
}

func testSelector(sc: ObjCSubclass, dm: DynamicMembers) {
  _ = #selector(sc.foo) // expected-warning{{argument of '#selector' refers to instance method 'foo()' in 'ObjCSubclass' that depends on '@objc' attribute inference deprecated in Swift 4}}
  _ = #selector(getter: dm.bar) // expected-warning{{argument of '#selector' refers to var 'bar' in 'DynamicMembers' that depends on '@objc' attribute inference deprecated in Swift 4}}
}

func testKeypath(dm: DynamicMembers) {
  _ = #keyPath(DynamicMembers.bar) // expected-warning{{argument of '#keyPath' refers to property 'bar' in 'DynamicMembers' that depends on '@objc' attribute inference deprecated in Swift 4}}
}

func testDynamicCalls(ao: AnyObject) {
  ao.foo?() // expected-warning{{reference to instance method 'foo()' of 'ObjCSubclass' depends on '@objc' attribute inference deprecated in Swift 4}}
  _ = ao.bar! // expected-warning{{reference to var 'bar' of 'ObjCSubclass' depends on '@objc' attribute inference deprecated in Swift 4}}
}
