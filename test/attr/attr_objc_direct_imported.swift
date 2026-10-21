// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// #selector, #keyPath and '@objc' protocol witnesses accept imported direct
// members until the feature is on.
// The notes land in the imported header, hence -verify-ignore-unrelated.

// RUN: %target-typecheck-verify-swift -import-objc-header %S/../Inputs/objc_direct.h
// RUN: %target-typecheck-verify-swift -import-objc-header %S/../Inputs/objc_direct.h \
// RUN:   -enable-experimental-feature ObjCDirect -verify-additional-prefix enabled- \
// RUN:   -verify-ignore-unrelated

import Foundation

func nameOnlyUses(_ bar: Bar) {
  _ = NSStringFromSelector(#selector(getter: Bar.directProperty))
  // expected-enabled-error@-1 {{whose accessors are direct}}
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
    // expected-enabled-error@-1 {{whose accessors are direct}}
  ]
}

// objc_direct_members category.
func fromDirectMembersCategory() {
  _ = #selector(Bar.directMethod2)
  // expected-enabled-error@-1 {{that is a direct method}}
  _ = #selector(getter: Bar.directProperty2)
  // expected-enabled-error@-1 {{whose accessors are direct}}
}

func keyPaths() -> [String] {
  return [
    #keyPath(Bar.directProperty),
    // expected-enabled-error@-1 {{that is direct}}
    #keyPath(Bar.directProperty2),
    // expected-enabled-error@-1 {{that is direct}}
  ]
}

@objc protocol RequiresMethod {
  func directMethod() -> String! // expected-enabled-note {{requirement 'directMethod()' declared here}}
}
@objc protocol HasOptionalMethod {
  @objc optional func directMethod2() -> String! // expected-enabled-note {{requirement 'directMethod2()' declared here}}
}
@objc protocol RequiresProperty {
  var directProperty: Int32 { get } // expected-enabled-note {{requirement 'directProperty' declared here}}
}

extension Bar: RequiresMethod {}
// expected-enabled-error@-1 {{instance method 'directMethod()' cannot satisfy a requirement of '@objc' protocol 'RequiresMethod' because it is a direct method}}
extension Bar: HasOptionalMethod {}
// expected-enabled-error@-1 {{instance method 'directMethod2()' cannot satisfy a requirement of '@objc' protocol 'HasOptionalMethod' because it is a direct method}}
extension Bar: RequiresProperty {}
// expected-enabled-error@-1 {{property 'directProperty' cannot satisfy a requirement of '@objc' protocol 'RequiresProperty' because it is direct}}
