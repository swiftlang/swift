// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_C.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/MemberImportVisibility -o %t %S/Inputs/MemberImportVisibility/Categories_E.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/MemberImportVisibility -import-objc-header %S/Inputs/MemberImportVisibility/Bridging.h -verify -swift-version 5
// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/MemberImportVisibility -import-objc-header %S/Inputs/MemberImportVisibility/Bridging.h -verify -swift-version 6
// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/MemberImportVisibility -import-objc-header %S/Inputs/MemberImportVisibility/Bridging.h -verify -swift-version 5 -enable-experimental-feature MemberImportVisibility -verify-additional-prefix member-visibility-

// REQUIRES: objc_interop

import Categories_B
import Categories_E

// expected-member-visibility-note@-1 2 {{add import of module 'Categories_C'}}{{1-1=internal import Categories_C\n}}
// expected-member-visibility-note@-2 {{add import of module 'Categories_D'}}{{1-1=internal import Categories_D\n}}
func test(x: X) {
  x.fromA()
  x.fromOverlayForA()
  x.fromB()
  x.fromOverlayForB()
  x.fromC() // expected-member-visibility-error {{class method 'fromC()' is not available due to missing import of defining module 'Categories_C'}}
  x.fromOverlayForC() // expected-member-visibility-error {{instance method 'fromOverlayForC()' is not available due to missing import of defining module 'Categories_C'}}
  x.fromSubmoduleOfD() // expected-member-visibility-error {{class method 'fromSubmoduleOfD()' is not available due to missing import of defining module 'Categories_D'}}
  x.fromBridgingHeader()
}

func testAnyObject(a: AnyObject) {
  a.fromA()
  a.fromOverlayForAObjC()
  a.fromB()
  a.fromOverlayForBObjC()
  // FIXME: Better diagnostics?
  // Name lookup for AnyObject already ignored transitive imports, so
  // `MemberImportVisibility` has no effect on these diagnostics.
  a.fromC() // expected-error {{value of type 'AnyObject' has no member 'fromC'}}
  a.fromOverlayForCObjC() // expected-error {{value of type 'AnyObject' has no member 'fromOverlayForCObjC'}}
  a.fromBridgingHeader()
}

extension StructInBridgingHeader {
  init(_ x: Int32) {
    self.init(member: x)
  }

  var wrappedMember: Int32 {
    return member
  }
}
