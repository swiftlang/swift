// RUN: %target-swift-frontend -typecheck -disable-objc-attr-requires-foundation-module -I %S/../Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import ObjCRuntimeVisible

extension A {
  @objc func foo() { } // expected-error{{instance method cannot be marked @objc because class 'A' is only visible via the Objective-C runtime}}
  func bar() {} // okay, implicitly non-objc
}

func test(x: AnyObject) {
  _ = x.bar() // expected-error {{value of type 'AnyObject' has no member 'bar'}}
}

class B : A { } // expected-error{{cannot inherit from class 'A' because it is only visible via the Objective-C runtime}}

protocol SwiftProto {}
@objc protocol ObjCProto {}

extension A: ObjCProto {} // expected-error {{class 'A' cannot conform to @objc protocol 'ObjCProto' because the class is only visible via the Objective-C runtime}}
extension A: SwiftProto {} // okay
