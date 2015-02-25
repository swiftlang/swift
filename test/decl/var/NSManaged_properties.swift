// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import Foundation

@objc class X {
  func foo() -> X { return self }
}

@NSManaged var global: Int // expected-error {{@NSManaged only allowed on a property in a class}}

@NSManaged     // expected-error {{@NSManaged may only be used on 'var' declarations}}
func managedFunction() {}

class SwiftGizmo : A {
  @NSManaged var a: X
  @NSManaged var b: Int
  @NSManaged let c: Int  // expected-error {{@NSManaged not allowed on a 'let' property}}

  @NSManaged class var d: Int = 4  // expected-error {{@NSManaged only allowed on a property in a class}} \
            // expected-error {{class stored properties not yet supported}}


  @NSManaged var e: Int { return 4 } // expected-error {{@NSManaged not allowed on computed properties}}

  @NSCopying @NSManaged var optionalProperty : NSString?  // expected-error {{@NSManaged property cannot also be marked @NSCopying}}

  override init() {}
}

