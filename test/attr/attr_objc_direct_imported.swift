// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// #selector accepts imported direct members until the feature is on.
// The notes land in the imported header, hence -verify-ignore-unrelated.

// RUN: %target-typecheck-verify-swift -import-objc-header %S/../Inputs/objc_direct.h
// RUN: %target-typecheck-verify-swift -import-objc-header %S/../Inputs/objc_direct.h \
// RUN:   -enable-experimental-feature ObjCDirect -verify-additional-prefix enabled- \
// RUN:   -verify-ignore-unrelated

import Foundation

func nameOnlyUses(_ bar: Bar) {
  _ = NSStringFromSelector(#selector(getter: Bar.directProperty))
  // expected-enabled-error@-1 {{that is a direct method}}
  _ = NSStringFromSelector(#selector(Bar.directMethod))
  // expected-enabled-error@-1 {{that is a direct method}}
}

func selectorReferences() -> [Selector] {
  return [
    #selector(Bar.directMethod),
    // expected-enabled-error@-1 {{that is a direct method}}
    #selector(Bar.directClassMethod),
    // expected-enabled-error@-1 {{that is a direct method}}
    #selector(getter: Bar.directProperty),
    // expected-enabled-error@-1 {{that is a direct method}}
  ]
}

// objc_direct_members category.
func fromDirectMembersCategory() {
  _ = #selector(Bar.directMethod2)
  // expected-enabled-error@-1 {{that is a direct method}}
  _ = #selector(getter: Bar.directProperty2)
  // expected-enabled-error@-1 {{that is a direct method}}
}
