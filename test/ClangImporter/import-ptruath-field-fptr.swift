// RUN: %swift-frontend %s  -dump-ast -target arm64e-apple-ios13.0 -I %S/Inputs/ 2>&1 | %FileCheck %s
// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

import PointerAuth

// CHECK:(declref_expr type='__ptrauth(1,0,50) @convention(c) () -> Int32' location={{.*}}test/ClangImporter/import-ptruath-field-fptr.swift:9:10{{.*}}

func test_field_fn_ptr() -> Int32 {
  let fn = ptr_to_secure_struct!.pointee.secure_func_ptr!
  return fn()
}
