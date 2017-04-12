// RUN: %target-typecheck-verify-swift -enable-experimental-subclass-existentials -disable-objc-attr-requires-foundation-module

@objc class ObjCClass {}
@objc protocol ObjCProtocol {}

class SwiftClass {}
protocol SwiftProtocol {}

class SomeMethods {

  @objc
  func canRepresentInObjC(x: ObjCClass & ObjCProtocol) {}

  @objc
  func cannotRepresentInObjC(x: SwiftClass & ObjCProtocol) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol-constrained type containing class 'SwiftClass' cannot be represented in Objective-C}}

  @objc
  func alsoCannotRepresentInObjC(x: ObjCClass & SwiftProtocol) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol-constrained type containing protocol 'SwiftProtocol' cannot be represented in Objective-C}}
}
