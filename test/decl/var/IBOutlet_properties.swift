// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/../../SILGen/Inputs %s -I %S/../../SILGen/Inputs -enable-source-import -parse -verify

import Foundation

@objc class C {
}

class SwiftGizmo {
  @IBOutlet var a: C
  @IBOutlet var b : C[]
  @IBOutlet var c : String
  @IBOutlet var d : String[]


  @IBOutlet var bad1 : Int  // expected-error {{'IBOutlet' property cannot have non-object type 'Int'}}

  init() {}
}

