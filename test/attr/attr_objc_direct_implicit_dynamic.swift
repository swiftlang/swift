// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect -enable-implicit-dynamic

import Foundation

// Under -enable-implicit-dynamic every method is 'dynamic', so none can be direct.
class ImplicitlyDynamic: NSObject {
  @objcDirect final func method() {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to a 'dynamic' instance method; 'dynamic' requires dispatch through the Objective-C runtime}}
}
