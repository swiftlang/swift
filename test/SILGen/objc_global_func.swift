// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -module-name objc_global_func | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// A top-level @objc function (SE-0495) uses one of two symbol models:
// - C-compatible signature: single C entry point (like @c)
// - ObjC-bridged signature: Swift body + C thunk (like @_cdecl)

// C-compatible: single symbol, no thunk.
// CHECK-LABEL: sil hidden [asmname "basic"] [ossa] @$s16objc_global_func5basicyyFTo : $@convention(c) () -> () {
@objc func basic() {}

// C-compatible: Int is trivially representable in ObjC.
// CHECK-LABEL: sil hidden [asmname "customName"] [ossa] @$s16objc_global_func5namedSiyFTo : $@convention(c) () -> Int {
@objc(customName) func named() -> Int { 0 }

// C-compatible: NSObject is trivially representable in ObjC.
// CHECK-LABEL: sil hidden [asmname "takesObject"] [ossa] @$s16objc_global_func11takesObjectyySo8NSObjectCFTo : $@convention(c) (NSObject) -> () {
@objc(takesObject) func takesObject(_ x: NSObject) {}

// C-compatible: all Int parameters.
// CHECK-LABEL: sil hidden [asmname "addNumbers"] [ossa] @$s16objc_global_func3add3lhs3rhsS2i_SitFTo : $@convention(c) (Int, Int) -> Int {
@objc(addNumbers) func add(lhs: Int, rhs: Int) -> Int { lhs + rhs }

// Bridged types: String <-> NSString requires a thunk.
// CHECK-LABEL: sil hidden [thunk] [asmname "greet"] [ossa] @$s16objc_global_func5greet4nameS2S_tFTo : $@convention(c) (NSString) -> @autoreleased NSString {
// CHECK-LABEL: sil hidden [ossa] @$s16objc_global_func5greet4nameS2S_tF : $@convention(thin) (@guaranteed String) -> @owned String {
@objc(greet) func greet(name: String) -> String { "Hi \(name)" }

// Bridged types: [NSObject] <-> NSArray requires a thunk.
// CHECK-LABEL: sil hidden [thunk] [asmname "count"] [ossa] @$s16objc_global_func5count5itemsSiSaySo8NSObjectCG_tFTo : $@convention(c) (NSArray) -> Int {
@objc(count) func count(items: [NSObject]) -> Int { items.count }

// Swift call sites: C-compatible functions call the C entry point;
// bridged functions call the native Swift body.
// CHECK-LABEL: sil hidden [ossa] @$s16objc_global_func6calleryyF
func caller() {
  basic()
  // CHECK: function_ref @$s16objc_global_func5basicyyFTo
  _ = named()
  // CHECK: function_ref @$s16objc_global_func5namedSiyFTo
  _ = add(lhs: 7, rhs: 11)
  // CHECK: function_ref @$s16objc_global_func3add3lhs3rhsS2i_SitFTo
  _ = greet(name: "World")
  // CHECK: function_ref @$s16objc_global_func5greet4nameS2S_tF
}
