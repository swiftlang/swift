// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/../../SILGen/Inputs %s -I %S/../../SILGen/Inputs -enable-source-import -parse -verify

import gizmo

@objc class X {
  func foo() -> X { return self }
}

@NSManaged var global: Int // expected-error {{'NSManaged' attribute only allowed on a property in a class}}

@NSManaged     // expected-error {{'NSManaged' attribute only allowed on a property in a class}}
func managedFunction() {}

class SwiftGizmo : Gizmo {
  @NSManaged var a: X
  @NSManaged var b: Int
  @NSManaged let c: Int  // expected-error {{'NSManaged' attribute not allowed on a 'let' property}}

  @NSManaged class var d: Int = 4  // expected-error {{'NSManaged' attribute only allowed on a property in a class}} \
            // expected-error {{class variables not yet supported}}


  @NSManaged var e: Int { return 4 } // expected-error {{'NSManaged' not allowed on computed properties}}

  init() {}
}

