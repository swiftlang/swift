// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir %s -module-name objc_global_func | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// A top-level @objc function (SE-0495) emits both a C-convention thunk (with
// the C symbol name) and a native Swift-convention body. Bridged types like
// String and Array are translated in the thunk (String <-> NSString, etc.).

// C thunk (called from C/ObjC):
// CHECK-DAG: define hidden void @basic()
// Native body (called from Swift):
// CHECK-DAG: define hidden swiftcc void @"$s16objc_global_func5basicyyF"()
@objc func basic() {}

// CHECK-DAG: define hidden i64 @customName()
@objc(customName) func named() -> Int { 0 }

// ObjC object parameters are passed as plain pointers in the C thunk.
// CHECK-DAG: define hidden void @takesObject(ptr %0)
@objc(takesObject) func takesObject(_ x: NSObject) {}

// Argument labels are dropped in the C signature; parameters are positional.
// CHECK-DAG: define hidden i64 @addNumbers(i64 %0, i64 %1)
@objc(addNumbers) func add(lhs: Int, rhs: Int) -> Int { lhs + rhs }

// Bridged types: the C thunk takes/returns NSString (ptr), the native body
// takes/returns String (i64 + ptr pair).
// CHECK-DAG: define hidden ptr @greet(ptr %0)
// CHECK-DAG: define hidden swiftcc { i64, ptr } @"$s16objc_global_func5greet4nameS2S_tF"(i64 %0, ptr %1)
@objc(greet) func greet(name: String) -> String { "Hi \(name)" }

// Bridged collection: the C thunk takes NSArray (ptr).
// CHECK-DAG: define hidden i64 @count(ptr %0)
@objc(count) func count(items: [NSObject]) -> Int { items.count }

// A Swift call uses swiftcc to the native body, not the C thunk.
// CHECK-LABEL: define hidden swiftcc void @"$s16objc_global_func6calleryyF"
// CHECK: call swiftcc i64 @"$s16objc_global_func5namedSiyF"()
// CHECK: call swiftcc i64 @"$s16objc_global_func3add3lhs3rhsS2i_SitF"(i64 7, i64 11)
func caller() {
  _ = named()
  _ = add(lhs: 7, rhs: 11)
}
