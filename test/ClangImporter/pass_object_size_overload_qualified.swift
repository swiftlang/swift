// RUN: %target-swift-frontend -I %S/Inputs/pass-object-size-module -primary-file %s -emit-ir -verify | %FileCheck %s --implicit-check-not=_Z6ovl_fpPcU17pass_object_size0

// A module-qualified reference is the one shape whose argument expression is not
// a plain DeclRefExpr -- it is a DotSyntaxBaseIgnoredExpr, which
// getSemanticsProvidingExpr() does not look through. Overload resolution still
// has to prefer the candidate without pass_object_size, which exercises the
// findSelectedOverloadFor path rather than the DeclRefExpr fast path.

// REQUIRES: PTRSIZE=64

import PassObjectSizeOverloads

typealias CFn = @convention(c) (UnsafeMutablePointer<CChar>?) -> Int

func takesCPointer(_ f: CFn) {}

// CHECK-LABEL: define hidden swiftcc void @"$s35pass_object_size_overload_qualified16testQualifiedRefyyF"
func testQualifiedRef() {
  // CHECK: call swiftcc void {{.*}}(ptr @_Z6ovl_fpPc)
  takesCPointer(PassObjectSizeOverloads.ovl_fp)
}
