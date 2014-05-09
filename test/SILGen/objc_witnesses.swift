// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation
import gizmo

protocol Fooable {
  func foo() -> String!
}

// Witnesses Fooable.foo with the original ObjC-imported -foo method .
extension Foo: Fooable {}

// CHECK-LABEL: sil @_TTWCSo3Foo14objc_witnesses7FooableFS1_3fooUS1___fRQPS1_FT_GSQSS_
// CHECK:         function_ref @_TTOFCSo3Foo3foofS_FT_GSQSS_
