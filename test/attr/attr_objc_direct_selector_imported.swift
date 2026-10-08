// REQUIRES: objc_interop

// The #selector check is gated on the ObjCDirect feature, and this file does
// not enable it: a module that never opted in keeps compiling. Taking the
// selector only for its name is correct even though it will never resolve to
// an implementation, and that idiom predates the attribute.
//
// attr_objc_direct_selector_imported_enabled.swift is the same imports with
// the feature on, where these are rejected.

// RUN: %target-typecheck-verify-swift -import-objc-header %S/../Inputs/objc_direct.h

import Foundation

func nameOnlyUses(_ bar: Bar) {
  _ = NSStringFromSelector(#selector(getter: Bar.directProperty))
  _ = NSStringFromSelector(#selector(Bar.directMethod))
}

func selectorReferences() -> [Selector] {
  return [
    #selector(Bar.directMethod),
    #selector(Bar.directClassMethod),
    #selector(getter: Bar.directProperty),
  ]
}

// objc_direct_members category.
func fromDirectMembersCategory() {
  _ = #selector(Bar.directMethod2)
  _ = #selector(getter: Bar.directProperty2)
}
