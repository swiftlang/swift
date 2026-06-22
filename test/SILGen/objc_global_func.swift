// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -module-name objc_global_func | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// A top-level @objc function (SE-0495) emits a native-to-foreign thunk
// carrying the [asmname] C symbol, plus a native Swift body. The thunk bridges
// Objective-C types to/from Swift types when needed.

// Thunk with the C symbol:
// CHECK-LABEL: sil hidden [thunk] [asmname "basic"] [ossa] @$s16objc_global_func5basicyyFTo : $@convention(c) () -> () {
// Native body:
// CHECK-LABEL: sil hidden [ossa] @$s16objc_global_func5basicyyF : $@convention(thin) () -> () {
@objc func basic() {}

// The C symbol name comes from the @objc(name) argument when present.
// CHECK-LABEL: sil hidden [thunk] [asmname "customName"] [ossa] @$s16objc_global_func5namedSiyFTo : $@convention(c) () -> Int {
@objc(customName) func named() -> Int { 0 }

// Objective-C class types are allowed in the signature.
// CHECK-LABEL: sil hidden [thunk] [asmname "takesObject"] [ossa] @$s16objc_global_func11takesObjectyySo8NSObjectCFTo : $@convention(c) (NSObject) -> () {
@objc(takesObject) func takesObject(_ x: NSObject) {}

// Argument labels are kept in the Swift name; the C symbol is flat.
// CHECK-LABEL: sil hidden [thunk] [asmname "addNumbers"] [ossa] @$s16objc_global_func3add3lhs3rhsS2i_SitFTo : $@convention(c) (Int, Int) -> Int {
@objc(addNumbers) func add(lhs: Int, rhs: Int) -> Int { lhs + rhs }

// Bridged types: the thunk bridges String <-> NSString automatically.
// CHECK-LABEL: sil hidden [thunk] [asmname "greet"] [ossa] @$s16objc_global_func5greet4nameS2S_tFTo : $@convention(c) (NSString) -> @autoreleased NSString {
@objc(greet) func greet(name: String) -> String { "Hi \(name)" }

// Bridged types: the thunk bridges [NSObject] <-> NSArray automatically.
// CHECK-LABEL: sil hidden [thunk] [asmname "count"] [ossa] @$s16objc_global_func5count5itemsSiSaySo8NSObjectCG_tFTo : $@convention(c) (NSArray) -> Int {
@objc(count) func count(items: [NSObject]) -> Int { items.count }

// Swift call sites reference the native body, not the thunk.
// CHECK-LABEL: sil hidden [ossa] @$s16objc_global_func6calleryyF
func caller() {
  basic()
  // CHECK: function_ref @$s16objc_global_func5basicyyF
  _ = named()
  // CHECK: function_ref @$s16objc_global_func5namedSiyF
  _ = add(lhs: 7, rhs: 11)
  // CHECK: function_ref @$s16objc_global_func3add3lhs3rhsS2i_SitF
}
