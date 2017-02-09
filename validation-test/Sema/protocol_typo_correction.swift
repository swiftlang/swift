// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -D LIB %s -o %t/Lib.swiftmodule
// RUN: %target-swift-frontend -I %t -typecheck %s -verify
// REQUIRES: objc_interop

#if LIB

import Foundation

@objc public protocol Proto {
  @objc optional func method(_: Int, for object: NSObject, dividing double: Double)
}

#else

import Foundation
import Lib

class Impl: Proto {
  func methodWithInt(_: Int, forObject object: NSObject, dividingDouble: Double) { }
  // expected-warning@-1 {{instance method 'methodWithInt(_:forObject:dividingDouble:)' nearly matches optional requirement 'method(_:for:dividing:)' of protocol 'Proto'}}
  // expected-note@-2{{rename to 'method(_:for:dividing:)' to satisfy this requirement}}{{8-21=method}}{{30-39=for}}{{58-58=dividing }}{{none}}
  // expected-note@-3{{move 'methodWithInt(_:forObject:dividingDouble:)' to an extension to silence this warning}}
  // expected-note@-4{{make 'methodWithInt(_:forObject:dividingDouble:)' private to silence this warning}}{{3-3=private }}
}

#endif
