// REQUIRES: objc_interop

// Imported objc_direct declarations are not diagnosed by the #selector check;
// attr_objc_direct_selector.swift covers the Swift-declared cases that are.
// Taking the selector only for its name is correct even though it will never
// resolve to an implementation.
//
// Deliberately does not enable ObjCDirect: the check is reachable without it,
// which is why diagnosing here would hit code that never opted in.

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
