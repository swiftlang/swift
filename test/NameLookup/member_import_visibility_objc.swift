// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_C.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_E.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/MemberImportVisibility -import-objc-header %S/Inputs/MemberImportVisibility/Bridging.h -verify -swift-version 5 -verify-additional-prefix no-member-visibility-
// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/MemberImportVisibility -import-objc-header %S/Inputs/MemberImportVisibility/Bridging.h -verify -swift-version 6 -verify-additional-prefix no-member-visibility-
// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/MemberImportVisibility -import-objc-header %S/Inputs/MemberImportVisibility/Bridging.h -verify -swift-version 5 -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-visibility-

// REQUIRES: objc_interop
// REQUIRES: swift_feature_MemberImportVisibility

import Categories_B
import Categories_E

// expected-member-visibility-note@-1 3 {{add import of module 'Categories_C'}}{{1-1=internal import Categories_C\n}}
// expected-member-visibility-note@-2 {{add import of module 'Categories_D'}}{{1-1=internal import Categories_D\n}}
func test(x: X) {
  x.fromA()
  x.fromOverlayForA()
  x.overriddenInOverlayForA() // expected-warning {{'overriddenInOverlayForA()' is deprecated: Categories_A.swift}}
  x.fromB()
  x.fromOverlayForB()
  x.overriddenInOverlayForB() // expected-warning {{'overriddenInOverlayForB()' is deprecated: Categories_B.swift}}
  x.fromC() // expected-member-visibility-error {{instance method 'fromC()' is not available due to missing import of defining module 'Categories_C'}}
  x.fromOverlayForC() // expected-member-visibility-error {{instance method 'fromOverlayForC()' is not available due to missing import of defining module 'Categories_C'}}
  x.overriddenInOverlayForC()
  // expected-no-member-visibility-warning@-1 {{'overriddenInOverlayForC()' is deprecated: Categories_C.swift}}
  // expected-member-visibility-warning@-2 {{'overriddenInOverlayForC()' is deprecated: Categories_A.h}}
  x.fromSubmoduleOfD() // expected-member-visibility-error {{instance method 'fromSubmoduleOfD()' is not available due to missing import of defining module 'Categories_D'}}
  x.fromBridgingHeader()
  x.overridesCategoryMethodOnNSObject()

  let subclassFromC = makeSubclassFromC()
  subclassFromC.overriddenInSubclassInOverlayForC()
  // expected-warning@-1 {{'overriddenInSubclassInOverlayForC()' is deprecated: Categories_C.swift}}
  // expected-member-visibility-error@-2 {{instance method 'overriddenInSubclassInOverlayForC()' is not available due to missing import of defining module 'Categories_C'}}
}

func testAnyObject(a: AnyObject) {
  a.fromA()
  a.fromOverlayForAObjC()
  a.overriddenInOverlayForA() // expected-warning {{'overriddenInOverlayForA()' is deprecated: Categories_A.h}}
  a.fromB()
  a.fromOverlayForBObjC()
  a.overriddenInOverlayForB() // expected-warning {{'overriddenInOverlayForB()' is deprecated: Categories_A.h}}
  // FIXME: Better diagnostics?
  // Name lookup for AnyObject already ignored transitive imports, so
  // `MemberImportVisibility` has no effect on these diagnostics.
  a.fromC() // expected-error {{value of type 'AnyObject' has no member 'fromC'}}
  a.fromOverlayForCObjC() // expected-error {{value of type 'AnyObject' has no member 'fromOverlayForCObjC'}}
  a.overriddenInOverlayForC() // expected-warning {{'overriddenInOverlayForC()' is deprecated: Categories_A.h}}
  a.fromBridgingHeader()
  a.overridesCategoryMethodOnNSObject()
}

extension StructInBridgingHeader {
  init(_ x: Int32) {
    self.init(member: x)
  }

  var wrappedMember: Int32 {
    return member
  }
}

extension ObjectInBridgingHeader {
  func test() {
    overridesCategoryMethodOnNSObject()
  }
}
