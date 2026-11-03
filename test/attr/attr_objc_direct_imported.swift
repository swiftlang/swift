// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule \
// RUN:   -module-name Lib -enable-experimental-feature ObjCDirect %t/Lib.swift

// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   -import-objc-header %S/../Inputs/objc_direct.h %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   -import-objc-header %S/../Inputs/objc_direct.h %t/main.swift \
// RUN:   -enable-experimental-feature ObjCDirect -verify-additional-prefix enabled- \
// RUN:   -verify-ignore-unrelated

// Without the feature: warnings, which '-Werror ObjCDirect' makes errors.
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   -import-objc-header %S/../Inputs/objc_direct.h %t/main.swift \
// RUN:   -Werror ObjCDirect -verify-additional-prefix enabled- \
// RUN:   -verify-ignore-unrelated

//--- Lib.swift

import Foundation

@objc public class C: NSObject {
  @objcDirect public final func f() {}
  @objcDirect public var q: Int = 0
}

//--- main.swift

import Foundation
import Lib

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

// A Swift '@objcDirect' member of another module.
func libraryUses() {
  _ = #selector(C.f)
  // expected-enabled-error@-1 {{argument of '#selector' refers to instance method 'f()' that is a direct method}}
  // expected-enabled-note@Lib.C.f:2 {{a direct method is not added to its class's Objective-C method list}}
  _ = #keyPath(C.q)
  // expected-enabled-error@-1 {{argument of '#keyPath' refers to property 'q' that is direct}}
  // expected-enabled-note@Lib.C.q:2 {{key-value coding cannot reach the property}}
}
