// Tests that with -conditional-runtime-records, IRGen marks class, struct,
// enum, protocol, and protocol conformance records as conditionally removable
// via !llvm.used.conditional metadata.

// REQUIRES: objc_interop

// RUN: %target-build-swift -Xfrontend -conditional-runtime-records %s -emit-ir -o - | %FileCheck %s

import Foundation

public class Class {
}

@objc public protocol ObjCAnnotatedProtocol {
}

// CHECK-DAG: @"__$s4main5ClassCMF_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main5ClassCMF", i64 1, ptr @"$s4main5ClassCMn" }, section "__DATA,__llvm_condlive", align 8
// CHECK-DAG: @"__$s4main21ObjCAnnotatedProtocol_pMF_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main21ObjCAnnotatedProtocol_pMF", i64 1, ptr @"$s4main21ObjCAnnotatedProtocolMp" }, section "__DATA,__llvm_condlive", align 8
// CHECK-DAG: @"__$s4main5ClassCHn_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main5ClassCHn", i64 1, ptr @"$s4main5ClassCMn" }, section "__DATA,__llvm_condlive", align 8
// CHECK-DAG: @"__objc_classes_$s4main5ClassCN_cl" = private constant { ptr, i64, ptr } { ptr @"objc_classes_$s4main5ClassCN", i64 1, ptr @"$s4main5ClassCN" }, section "__DATA,__llvm_condlive", align 8

// CHECK:      @llvm.{{(compiler.)?}}used = appending global [
// CHECK-SAME:   @"$s4main5ClassCHn"
// CHECK-SAME: ], section "llvm.metadata"

