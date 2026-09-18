// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect -enable-implicit-dynamic

import Foundation

// '-enable-implicit-dynamic' makes every eligible method 'dynamic', and
// 'dynamic' requires dispatch through the Objective-C runtime. The two options
// are therefore mutually exclusive, not merely interacting: under this flag no
// method can carry @objcDirect at all.
//
// This lives in its own file rather than as a second RUN line on
// attr_objc_direct_dispatch.swift because the 'dynamic' rule fires first and
// masks the others -- an @IBAction method would report the dynamic conflict
// instead of its own, and the protocol-witness error would disappear entirely
// because the attribute has already been stripped by then.
//
// Note this is caught only because the rule asks isDynamic() rather than
// hasAttribute<DynamicAttr>(); the implicit attribute is added by
// IsDynamicRequest, not written in the source.
class ImplicitlyDynamic: NSObject {
  @objcDirect final func method() {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to a 'dynamic' method; 'dynamic' requires dispatch through the Objective-C runtime}}
}
