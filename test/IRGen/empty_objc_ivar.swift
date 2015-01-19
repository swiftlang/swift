// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -emit-ir -verify -sdk %S/Inputs -I %t %s

import Foundation

class EmptyIVar: NSObject {
  var foo: ()
}

