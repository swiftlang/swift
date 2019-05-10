// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I %t -module-name SomeModule -emit-module -emit-module-path=%t/SomeModule.swiftmodule %S/Inputs/objc_dynamic_replacement.swift -enable-private-imports -swift-version 5 -enable-implicit-dynamic
// RUN: %target-swift-emit-silgen -I %t %s -swift-version 5 | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
@_private(sourceFile: "Foo.swift") import SomeModule

extension Object {
// CHECK-DAG: sil hidden [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE0F0yyF
// CHECK-DAG: sil hidden [thunk] [objc_replacement_for "implicitObjCMethod"] [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE0F0yyFTo
  @_dynamicReplacement(for: implicitObjCMethod())
  func replacement() {
  }

// CHECK-DAG: sil hidden [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE12replacement2yyF
// CHECK-DAG: sil hidden [thunk] [objc_replacement_for "objCMethod"] [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE12replacement2yyFTo
  @_dynamicReplacement(for: objCMethod())
  func replacement2() {
  }

// CHECK-DAG: sil hidden [thunk] [objc_replacement_for "objcProperty"] [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE12replacement3SivgTo
// CHECK-DAG: sil hidden [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE12replacement3Sivg
  @_dynamicReplacement(for: objcProperty)
  var replacement3 : Int {
    return 2
  }

// CHECK-DAG: sil hidden [thunk] [objc_replacement_for "objcProperty2"] [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE12replacement4SivgTo
// CHECK-DAG: sil hidden [ossa] @$s10SomeModule6ObjectC24objc_dynamic_replacementE12replacement4Sivg
  @_dynamicReplacement(for: objcProperty2)
  var replacement4 : Int {
    get {
      return 2
    }
    set {
    }
  }
}
