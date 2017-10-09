// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -o - -primary-file %s -swift-version 3 -I %S/Inputs/custom-modules/

// REQUIRES: objc_interop

import Foundation
import SubclassExistentialsExtra

// FIXME: Consider better diagnostics here.

class SwiftLaundryService : NSLaundry {
// expected-error@-1 {{type 'SwiftLaundryService' does not conform to protocol 'NSLaundry'}}
  var g: (Garment & Coat)? = nil

  func wash(_ g: Garment & Coat) { // expected-note {{candidate has non-matching type '(Coat & Garment) -> ()'}}
    self.g = g
  }

  func bleach(_ g: Garment & Coat & Cotton) {} // expected-note {{candidate has non-matching type '(Coat & Cotton & Garment) -> ()'}}

  func dry() -> Garment & Coat { // expected-note {{candidate has non-matching type '() -> Coat & Garment'}}
    return g!
  }
}

class OldSwiftLaundryService : NSLaundry {
  var g: Coat? = nil

  func wash(_ g: Coat) {
    self.g = g
  }

  func bleach(_ g: Coat) {}

  func dry() -> Coat {
    return g!
  }
}

func testInheritanceFromComposition(_ object: CompositionSubObject, _ specific: CompositionSubSpecific) {
  let _: NSObject = object
  let _: NSCopying = object

  let _: SomeSpecificSubclass = specific
  let _: NSCopying = specific
}
