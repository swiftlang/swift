// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect

import Foundation

// The hazard is only reachable for internal and public methods: @objcDirect
// already rejects anything below internal (objc_direct_access_level), so
// private selector targets are self-protecting.
// The explanatory note attaches to the *declaration*, not to the use site, so a
// method named by two #selector expressions produces two notes on one line.
@objc class SelectorTarget: NSObject {
  // expected-note@+1 2 {{a direct method is not added to its class's Objective-C method list}}
  @objcDirect final func directHandler() {}
  // expected-note@+1 {{a direct method is not added to its class's Objective-C method list}}
  @objcDirect public final func publicDirectHandler() {}

  @objc final func normalHandler() {}
}

func formSelectors(_ target: SelectorTarget) {
  // A direct method is absent from the class's method list, so this would fail
  // at runtime rather than at build time.
  _ = #selector(SelectorTarget.directHandler)
  // expected-error@-1 {{argument of '#selector' refers to instance method 'directHandler()' that is a direct method}}

  _ = #selector(SelectorTarget.publicDirectHandler)
  // expected-error@-1 {{argument of '#selector' refers to instance method 'publicDirectHandler()' that is a direct method}}

  // Also rejected through an instance reference, not just the type.
  _ = #selector(target.directHandler)
  // expected-error@-1 {{argument of '#selector' refers to instance method 'directHandler()' that is a direct method}}

  // Control: a sibling @objc method keeps its selector, so this is fine. If the
  // check over-fired this line would produce an unexpected error.
  _ = #selector(SelectorTarget.normalHandler)
}
