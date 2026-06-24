// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir %s -module-name objc_global_func | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// A top-level @objc function (SE-0495) uses one of two symbol models:
// - C-compatible signature: single C entry point (like @c)
// - ObjC-bridged signature: C thunk + native Swift body (like @_cdecl)

// C-compatible: single C entry point, no separate Swift symbol.
// CHECK-DAG: define hidden i64 @customName()
@objc(customName) func named() -> Int { 0 }

// C-compatible: ObjC object types are trivially representable.
// CHECK-DAG: define hidden void @takesObject(ptr %0)
@objc(takesObject) func takesObject(_ x: NSObject) {}

// C-compatible: all Int parameters, single symbol.
// CHECK-DAG: define hidden i64 @addNumbers(i64 %0, i64 %1)
@objc(addNumbers) func add(lhs: Int, rhs: Int) -> Int { lhs + rhs }

// Bridged types: C thunk takes/returns NSString (ptr), native body
// takes/returns String (i64 + ptr pair).
// CHECK-DAG: define hidden ptr @greet(ptr %0)
// CHECK-DAG: define hidden swiftcc { i64, ptr } @"$s16objc_global_func5greet4nameS2S_tF"(i64 %0, ptr %1)
@objc(greet) func greet(name: String) -> String { "Hi \(name)" }

// Bridged collection: C thunk takes NSArray (ptr).
// CHECK-DAG: define hidden i64 @count(ptr %0)
@objc(count) func count(items: [NSObject]) -> Int { items.count }

// A Swift call to a C-compatible @objc function calls the C symbol directly.
// A Swift call to a bridged @objc function calls the native Swift body.
// CHECK-LABEL: define hidden swiftcc void @"$s16objc_global_func6calleryyF"
// CHECK: call i64 @addNumbers(i64 7, i64 11)
// CHECK: call swiftcc { i64, ptr } @"$s16objc_global_func5greet4nameS2S_tF"
func caller() {
  _ = add(lhs: 7, rhs: 11)
  _ = greet(name: "World")
}

// ABI stability: @c and @objc with the same C-compatible signature produce
// identical symbol layout -- a single C entry point with no Swift mangled name.
// This means changing between @c and @objc is ABI-stable per SE-0495.

// CHECK-DAG: define hidden i64 @viaCAttr(i64 %0)
// CHECK-NOT: define {{.*}} @"$s16objc_global_func7viaCFuncSiSiF"
@c(viaCAttr) func viaCFunc(x: Int) -> Int { x }

// CHECK-DAG: define hidden i64 @viaObjCAttr(i64 %0)
// CHECK-NOT: define {{.*}} @"$s16objc_global_func10viaObjCFuncSiSiF"
@objc(viaObjCAttr) func viaObjCFunc(x: Int) -> Int { x }

// Function type parameters: C function pointers and blocks are trivially
// representable in ObjC and use the single-symbol model. Swift closures
// are StaticBridged (closure -> block conversion) and need a thunk.
// Each @objc variant is paired with an @c equivalent to show ABI equivalence.

// C function pointer: trivially representable, single symbol.
// CHECK-DAG: define hidden void @takesCFuncPtr_c(ptr %0)
// CHECK-NOT: define {{.*}} @"$s16objc_global_func014takesCFuncPtr_D0yyyXCF"
@c(takesCFuncPtr_c) func takesCFuncPtr_c(_ f: @convention(c) () -> Void) { f() }

// CHECK-DAG: define hidden void @takesCFuncPtr_objc(ptr %0)
// CHECK-NOT: define {{.*}} @"$s16objc_global_func017takesCFuncPtr_obD0yyyXCF"
@objc(takesCFuncPtr_objc) func takesCFuncPtr_objc(_ f: @convention(c) () -> Void) { f() }

// ObjC block: trivially representable (Object kind), single symbol.
// CHECK-DAG: define hidden void @takesBlock_c(ptr %0)
// CHECK-NOT: define {{.*}} @"$s16objc_global_func012takesBlock_D0yyyXBF"
@c(takesBlock_c) func takesBlock_c(_ f: @convention(block) () -> Void) { f() }

// CHECK-DAG: define hidden void @takesBlock_objc(ptr %0)
// CHECK-NOT: define {{.*}} @"$s16objc_global_func015takesBlock_obcD0yyyXBF"
@objc(takesBlock_objc) func takesBlock_objc(_ f: @convention(block) () -> Void) { f() }

// Swift closure: StaticBridged (needs closure-to-block conversion), thunk model.
// Only valid with @objc -- @c rejects Swift closures at type-checking.
// CHECK-DAG: define hidden void @takesClosure(ptr %0)
// CHECK-DAG: define hidden swiftcc void @"$s16objc_global_func12takesClosureyyyyXEF"
@objc(takesClosure) func takesClosure(_ f: () -> Void) { f() }
