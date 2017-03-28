// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 4 -enable-source-import -I %S/Inputs
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
}

class DynamicMembers {
  dynamic func foo() { }
  
  dynamic var bar: NSObject? = nil // expected-note 2{{add '@objc' to expose this var to Objective-C}}
}

func test(sc: ObjCSubclass, dm: DynamicMembers) {
  _ = #selector(sc.foo) // expected-error{{argument of '#selector' refers to instance method 'foo()' that is not exposed to Objective-C}}

  // FIXME: should be errors, once 'dynamic' is updated
  _ = #selector(getter: dm.bar) // expected-warning{{argument of '#selector' refers to var 'bar' in 'DynamicMembers' that depends on '@objc' attribute inference deprecated in Swift 4}}
  _ = #keyPath(DynamicMembers.bar) // expected-warning{{argument of '#keyPath' refers to property 'bar' in 'DynamicMembers' that depends on '@objc' attribute inference deprecated in Swift 4}}
}
