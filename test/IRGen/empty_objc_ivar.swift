// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %swift -emit-ir -verify -target x86_64-apple-macosx10.9 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I %t %s
// XFAIL: linux

import Foundation

class EmptyIVar: NSObject {
  var foo: ()
}

