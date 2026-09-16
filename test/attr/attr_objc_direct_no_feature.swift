// REQUIRES: objc_interop

// Reject @objcDirect unless the experimental feature is enabled.
// RUN: %target-typecheck-verify-swift

import Foundation

@objc
class NoFeatureClass: NSObject {
  @objcDirect
  public final func foo() { return }
  // expected-error@-2 {{'objcDirect' attribute is only valid when experimental feature ObjCDirect is enabled}}

  // Checked before the other applicability rules, so a method that is invalid
  // for another reason still reports the missing feature.
  @objcDirect
  public func nonFinal() { return }
  // expected-error@-2 {{'objcDirect' attribute is only valid when experimental feature ObjCDirect is enabled}}
}
