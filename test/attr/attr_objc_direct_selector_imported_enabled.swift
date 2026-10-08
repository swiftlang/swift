// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// The same imports as attr_objc_direct_selector_imported.swift, with the
// feature on. Opting in extends the #selector check to imported objc_direct
// declarations, which are no more sendable than Swift's own.
//
// Matched on the tail of the message only: attr_objc_direct_selector.swift
// pins the full wording, and how each accessor is described is beside the
// point here.

// The explanatory note lands on the imported declaration, so it is out of
// buffer; -verify-ignore-unrelated covers that.

// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -enable-experimental-feature ObjCDirect -import-objc-header %S/../Inputs/objc_direct.h

import Foundation

func nameOnlyUses(_ bar: Bar) {
  _ = NSStringFromSelector(#selector(getter: Bar.directProperty))
  // expected-error@-1 {{that is a direct method}}
  _ = NSStringFromSelector(#selector(Bar.directMethod))
  // expected-error@-1 {{that is a direct method}}
}

func selectorReferences() -> [Selector] {
  return [
    #selector(Bar.directMethod),
    // expected-error@-1 {{that is a direct method}}
    #selector(Bar.directClassMethod),
    // expected-error@-1 {{that is a direct method}}
    #selector(getter: Bar.directProperty),
    // expected-error@-1 {{that is a direct method}}
  ]
}

// objc_direct_members category.
func fromDirectMembersCategory() {
  _ = #selector(Bar.directMethod2)
  // expected-error@-1 {{that is a direct method}}
  _ = #selector(getter: Bar.directProperty2)
  // expected-error@-1 {{that is a direct method}}
}
