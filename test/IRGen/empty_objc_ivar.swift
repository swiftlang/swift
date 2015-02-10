// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-ir -verify %s

// REQUIRES: objc_interop

import Foundation

class EmptyIVar: NSObject {
  var foo: ()
}

