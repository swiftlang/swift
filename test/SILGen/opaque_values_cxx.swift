// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -Xllvm -sil-full-demangle -cxx-interoperability-mode=swift-5.9 -import-objc-header %S/Inputs/opaque_values_cxx.h -primary-file %s | %FileCheck %s --check-prefix=CHECK

// TODO: Find the relevant mangled name instead of
// $sSo3stdO3__1O0020___wrap_iter__udAAdDaVSQSCSQ2eeoiySbx_xtFZTW for other
// platforms.
// REQUIRES: VENDOR=apple

import Cxx

// CHECK-LABEL: sil {{.*}}[ossa] @$sSo3stdO3__1O0065vectorCUnsignedIntstd__1allocatorCUnsignedInt_dDGIrdqahddCJdFaAjaV3Cxx0B8SequenceSCAgHP13__beginUnsafe11RawIteratorQzyFTW : {{.*}} {
// CHECK:       bb0([[VECTOR_ADDR:%[^,]+]] :
// CHECK:         [[VECTOR:%[^,]+]] = load_borrow [[VECTOR_ADDR]]
// CHECK:         [[BEGIN_FN:%[^,]+]] = function_ref
// CHECK:         [[BEGIN:%[^,]+]] = apply [[BEGIN_FN]]([[VECTOR]])
// CHECK:         end_borrow [[VECTOR]]
// CHECK:         return [[BEGIN]]
// CHECK-LABEL: } // end sil function '$sSo3stdO3__1O0065vectorCUnsignedIntstd__1allocatorCUnsignedInt_dDGIrdqahddCJdFaAjaV3Cxx0B8SequenceSCAgHP13__beginUnsafe11RawIteratorQzyFTW'
// CHECK-LABEL: sil {{.*}}[ossa] @$sSo3stdO{{(3__1O)?}}0047___wrap_iterUnsafePointerCUnsignedInt_heCInnaEgaVSQSCSQ2eeoiySbx_xtFZTW : {{.*}} {
// CHECK:       bb0([[LHS:%[^,]+]] : $std.__1.__wrap_iter<UnsafePointer<CUnsignedInt>>, [[RHS:%[^,]+]] :
// CHECK:         [[CALLEE:%[^,]+]] = function_ref @_ZNSt3__1eqB8ne{{.*}}IPKjEEbRKNS_11__wrap_iterIT_EES7_
// CHECK:         [[EQUAL:%[^,]+]] = apply [[CALLEE]]([[LHS]], [[RHS]])
// CHECK:         return [[EQUAL]]
// CHECK-LABEL: } // end sil function '$sSo3stdO{{(3__1O)?}}0047___wrap_iterUnsafePointerCUnsignedInt_heCInnaEgaVSQSCSQ2eeoiySbx_xtFZTW'
func test_cxx_vector_uint32t_iterate(_ n: Int, _ vectorOfU32: VectorOfU32) {
  for x in vectorOfU32 {}
}
