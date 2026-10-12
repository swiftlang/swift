// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-objc-attr-requires-foundation-module -print-ast %s 2>&1 | %FileCheck %s
// REQUIRES: objc_interop

import ObjectiveC

class Target: NSObject {
  @objc func method() {}
  @objc var prop: Int = 0
}

let methodSel = #selector(Target.method)
// CHECK: @_hasInitialValue internal let methodSel: Selector = #selector(Target.method)

let getterSel = #selector(getter: Target.prop)
// CHECK: @_hasInitialValue internal let getterSel: Selector = #selector(getter: Target.prop)

let setterSel = #selector(setter: Target.prop)
// CHECK: @_hasInitialValue internal let setterSel: Selector = #selector(setter: Target.prop)
