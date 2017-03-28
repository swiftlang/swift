// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 4 -enable-source-import -I %S/Inputs
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
}

class DynamicMembers {
  dynamic func foo() { } // expected-error{{'dynamic' instance method 'foo()' must also be '@objc'}}{{3-3=@objc }}
  
  dynamic var bar: NSObject? = nil
 // expected-error@-1{{'dynamic' var 'bar' must also be '@objc'}}{{3-3=@objc }}
}

func test(sc: ObjCSubclass, dm: DynamicMembers) {
  _ = #selector(sc.foo) // expected-error{{argument of '#selector' refers to instance method 'foo()' that is not exposed to Objective-C}}

  _ = #selector(getter: dm.bar)
  _ = #keyPath(DynamicMembers.bar)
}
