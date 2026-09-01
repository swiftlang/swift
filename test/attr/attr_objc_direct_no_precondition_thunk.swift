// REQUIRES: objc_interop

// Reject @objcDirect if `-fobjc-direct-precondition-thunk` is not present.
// RUN: %target-typecheck-verify-swift

import Foundation

@objc
class NoPreconditionThunkClass: NSObject {
  @objcDirect
  public final func foo() { return }
  // expected-error@-2 {{'@objcDirect' requires '-fobjc-direct-precondition-thunk'}}

  // Checked before the other applicability rules, so a method that is invalid
  // for another reason still reports the missing flag.
  @objcDirect
  public func nonFinal() { return }
  // expected-error@-2 {{'@objcDirect' requires '-fobjc-direct-precondition-thunk'}}
}
