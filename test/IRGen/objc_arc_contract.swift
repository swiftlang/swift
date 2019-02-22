// Make sure that we run objc arc contract when emitting ir or bc with optimization enabled.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir -Xllvm -disable-objc-arc-contract -parse-as-library -O | %FileCheck --check-prefix=CHECK-WITHOUT-PASS %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-bc -Xllvm -disable-objc-arc-contract -parse-as-library -O -o %t/test1.bc && %llvm-dis -o - %t/test1.bc | %FileCheck --check-prefix=CHECK-WITHOUT-PASS %s

// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir -parse-as-library -O | %FileCheck --check-prefix=CHECK-WITH-PASS %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-bc -parse-as-library -O -o %t/test2.bc && %llvm-dis -o - %t/test2.bc | %FileCheck --check-prefix=CHECK-WITH-PASS %s


// REQUIRES: objc_interop
// REQUIRES: asserts

// CHECK-WITHOUT-PASS: call void (...) @llvm.objc.clang.arc.use
// CHECK-WITH-PASS-NOT: call void (...) @llvm.objc.clang.arc.use

import Foundation

@inline(never)
public func foo() throws {
  let x: FileManager! = nil
  let y = URL(string: "http://swift.org")
  let z: URL! = nil
  let w: String = "foo"
  var e: NSError? = nil
  test(x, y, z, w, .usingNewMetadataOnly, &e)
}
