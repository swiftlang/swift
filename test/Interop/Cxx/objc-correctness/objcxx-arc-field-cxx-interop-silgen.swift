// RUN: %target-swift-frontend -I %S/Inputs -emit-silgen -cxx-interoperability-mode=default -Xcc -fignore-exceptions -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

// ARC-only C++ structs are already loadable without any special handling.
//
// Two independent mechanisms converge on this result:
//
// 1. classifyNonTrivialRecord (ImportDecl.cpp) is never reached for C++ types.
//    The outer guard, isNonTrivialToPrimitiveCopy(), is a C-level primitive
//    copy flag that Clang does not set on CXXRecordDecls. C++ non-triviality
//    is tracked through special members instead. So the entire classification
//    block, including the C++ interop guard that checks
//    hasNonTrivialCopyConstructor, is bypassed.
//
// 2. isCxxNonTrivial (TypeLowering.cpp) is set from canPassInRegisters().
//    Clang marks __strong fields as "trivial for call" in both ObjC and ObjC++
//    (HasTrivialSpecialMembersForCall in DeclCXX.cpp is not cleared for
//    OCL_Strong). So canPassInRegisters() returns true, isCxxNonTrivial is
//    false, and TypeLowering does not force address-only.
//
// The C and C++ import paths naturally produce the same loadable result for
// structs whose only non-triviality comes from __strong ARC fields.

import Foundation
import CxxClassWithNSStringInit

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testCxxArcStructIsLoadable
// CHECK: load [copy]
// CHECK: destroy_value
// CHECK: } // end sil function
func testCxxArcStructIsLoadable() {
  var s = S()
  s.A = "hello" as NSString
  _ = s
}
