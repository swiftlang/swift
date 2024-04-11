// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/Categories -o %t %S/Inputs/Categories/Categories_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/Categories -o %t %S/Inputs/Categories/Categories_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/Categories -o %t %S/Inputs/Categories/Categories_C.swift
// RUN: %target-swift-frontend -emit-module -I %t -I %S/Inputs/Categories -o %t %S/Inputs/Categories/Categories_E.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/Categories -verify -enable-experimental-feature ExtensionImportVisibility

// REQUIRES: objc_interop

import Categories_B
import Categories_E

// expected-note@-1 2 {{add import of module 'Categories_C'}}{{1-1=import Categories_C\n}}
// expected-note@-2 {{add import of module 'Categories_D'}}{{1-1=import Categories_D\n}}
func test(x: X) {
  x.fromA()
  x.fromOverlayForA()
  x.fromB()
  x.fromOverlayForB()
  x.fromC() // expected-error {{class method 'fromC()' is not available due to missing import of defining module 'Categories_C'}}
  x.fromOverlayForC() // expected-error {{instance method 'fromOverlayForC()' is not available due to missing import of defining module 'Categories_C'}}
  x.fromSubmoduleOfD() // expected-error {{class method 'fromSubmoduleOfD()' is not available due to missing import of defining module 'Categories_D'}}
}

func testAnyObject(a: AnyObject) {
  a.fromA()
  a.fromOverlayForAObjC()
  a.fromB()
  a.fromOverlayForBObjC()
  // FIXME: Better diagnostics?
  // Name lookup for AnyObject already ignored transitive imports, so
  // ExtensionImportVisibility has no effect on these diagnostics.
  a.fromC() // expected-error {{value of type 'AnyObject' has no member 'fromC'}}
  a.fromOverlayForCObjC() // expected-error {{value of type 'AnyObject' has no member 'fromOverlayForCObjC'}}
}
