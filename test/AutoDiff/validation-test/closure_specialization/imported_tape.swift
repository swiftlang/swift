// Specialize tapes imported from another module. Both client primary files
// specialize importedBranch<Int>, so their synthesized enum metadata must link.

// Keep the final SIL checks available in compile-only test runs.
// REQUIRES: executable_test || nonexecutable_test
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -c -parse-as-library -module-name TapeLibrary -emit-module-path %t/TapeLibrary.swiftmodule %S/../../Inputs/closure_specialization/tape_library.swift -o %t/TapeLibrary.o
// RUN: %target-swift-frontend -O -sil-verify-all -parse-as-library -module-name src -I %t %S/Inputs/imported_tape_other.swift -primary-file %s -emit-sil -o %t/client.sil
// RUN: %FileCheck %s < %t/client.sil
// RUN: %if executable_test %{ \
// RUN:   %target-build-swift -O -Xfrontend -sil-verify-all -parse-as-library -module-name src -I %t %S/Inputs/imported_tape_other.swift %s %t/TapeLibrary.o -o %t/client && \
// RUN:   %target-codesign %t/client && \
// RUN:   %target-run %t/client \
// RUN: %}

// Specialized branch-tracing enum for importedBranch<Int>.
// CHECK-LABEL:     {{^}}enum _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_bc69f444a66f75148dc73a16df7835f9 {
// CHECK-NEXT:        case bb2((predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb2__Pred__src_0_wrt_0_l<Int>, ()))
// CHECK:             case bb1((predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb1__Pred__src_0_wrt_0_l<Int>, (Float, Float)))
// CHECK-LABEL:     {{^}}}

// Specialized branch-tracing enum for importedBranch<Double>.
// CHECK-LABEL:     {{^}}enum _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_0c5710071e58d1aac1e33e7e4852d7eb {
// CHECK-NEXT:        case bb2((predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb2__Pred__src_0_wrt_0_l<Double>, ()))
// CHECK:             case bb1((predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb1__Pred__src_0_wrt_0_l<Double>, (Float, Float)))
// CHECK-LABEL:     {{^}}}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of clientInt(_:_:)
// CHECK:           sil @$s3src9clientIntyS2f_SbtFTJrSUpSr :
// CHECK:             = tuple $(predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb1__Pred__src_0_wrt_0_l<Int>, (Float, Float))
// CHECK:           bb[[#]](%[[#VALUE:]] : $Float, %[[#TAPE:]] : $_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_bc69f444a66f75148dc73a16df7835f9):
// CHECK:             // function_ref specialized pullback of clientInt(_:_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src9clientIntyS2f_SbtFTJpSUpSr066$s11TapeLibrary14importedBranchyS2f_xSbtlFlTJpSUUpSrSi_Tg500205_58defgh2_xij3TJrlmn18_Tg5$s3src059_AD__defgh2_xI97F_bb3__Pred__A60_0_wrt_0_l_spec_bb2_1_bb1_1_bc69f444a66f75148dc73a16df7835f9OD_FIJFIfwngtfTf1nb_nAA01_r2__defgh2_xi2F_s2__t2__a3_0_v5_0_l_w1_x3_1_y3_1_Z0OTf1nE_n :
// CHECK-NEXT:        %[[#BOUND:]] = partial_apply [callee_guaranteed] %[[#PULLBACK]](%[[#TAPE]]) : $@convention(thin) (Float, _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_bc69f444a66f75148dc73a16df7835f9) -> Float
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#VALUE]], %[[#BOUND]])
// CHECK-NEXT:        return %[[#RESULT]]

// CHECK-LABEL:     {{^}}// specialized pullback of clientInt(_:_:)
// CHECK:           bb0(%[[#]] : $Float, %[[#TAPE:]] : $_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_bc69f444a66f75148dc73a16df7835f9):
// CHECK-NOT:         = apply {{.*}} : $@callee_guaranteed
// CHECK:             switch_enum %[[#TAPE]], case #_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_bc69f444a66f75148dc73a16df7835f9.bb2!enumelt: bb[[#]], case #_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_bc69f444a66f75148dc73a16df7835f9.bb1!enumelt: bb[[#]]
// CHECK-NOT:         = apply {{.*}} : $@callee_guaranteed
// CHECK:           bb[[#]](%[[#]] : $(predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb1__Pred__src_0_wrt_0_l<Int>, (Float, Float))):
// CHECK-NOT:         = apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// reverse-mode derivative of clientDouble(_:_:)
// CHECK:           sil @$s3src12clientDoubleyS2f_SbtFTJrSUpSr :
// CHECK:             = tuple $(predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb1__Pred__src_0_wrt_0_l<Double>, (Float, Float))
// CHECK:           bb[[#]](%[[#VALUE:]] : $Float, %[[#TAPE:]] : $_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_0c5710071e58d1aac1e33e7e4852d7eb):
// CHECK:             // function_ref specialized pullback of clientDouble(_:_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src12clientDoubleyS2f_SbtFTJpSUpSr066$s11TapeLibrary14importedBranchyS2f_xSbtlFlTJpSUUpSrSd_Tg500205_58defgh2_xij3TJrlmn18_Tg5$s3src059_AD__defgh2_xI97F_bb3__Pred__A60_0_wrt_0_l_spec_bb2_1_bb1_1_0c5710071e58d1aac1e33e7e4852d7ebOD_FIJFIfwngtfTf1nb_nAA01_r2__defgh2_xi2F_s2__t2__a3_0_v5_0_l_w1_x3_1_y4_1_0Z0OTf1nE_n :
// CHECK-NEXT:        %[[#BOUND:]] = partial_apply [callee_guaranteed] %[[#PULLBACK]](%[[#TAPE]]) : $@convention(thin) (Float, _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_0c5710071e58d1aac1e33e7e4852d7eb) -> Float
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#VALUE]], %[[#BOUND]])
// CHECK-NEXT:        return %[[#RESULT]]

// CHECK-LABEL:     {{^}}// specialized pullback of clientDouble(_:_:)
// CHECK:           bb0(%[[#]] : $Float, %[[#TAPE:]] : $_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_0c5710071e58d1aac1e33e7e4852d7eb):
// CHECK-NOT:         = apply {{.*}} : $@callee_guaranteed
// CHECK:             switch_enum %[[#TAPE]], case #_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_0c5710071e58d1aac1e33e7e4852d7eb.bb2!enumelt: bb[[#]], case #_AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_0c5710071e58d1aac1e33e7e4852d7eb.bb1!enumelt: bb[[#]]
// CHECK-NOT:         = apply {{.*}} : $@callee_guaranteed
// CHECK:           bb[[#]](%[[#]] : $(predecessor: _AD__$s11TapeLibrary14importedBranchyS2f_xSbtlF_bb1__Pred__src_0_wrt_0_l<Double>, (Float, Float))):
// CHECK-NOT:         = apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

import _Differentiation
import TapeLibrary

@differentiable(reverse, wrt: x)
public func clientInt(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Int(1), flag)
}

@differentiable(reverse, wrt: x)
public func clientDouble(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Double(1), flag)
}

@main
struct ImportedTapeTests {
  static func main() {
    for flag in [true, false] {
      let integer = valueWithPullback(at: Float(3), of: { clientInt($0, flag) })
      let floating = valueWithPullback(at: Float(3), of: { clientDouble($0, flag) })
      let other = valueWithPullback(at: Float(3), of: { clientOther($0, flag) })
      precondition(integer.value == (flag ? 9 : 6))
      precondition(floating.value == (flag ? 9 : 6))
      precondition(other.value == (flag ? 9 : 6))
      for seed in [Float(1), 5, 2] {
        let expected = (flag ? 6 : 2) * seed
        precondition(integer.pullback(seed) == expected)
        precondition(floating.pullback(seed) == expected)
        precondition(other.pullback(seed) == expected)
      }
    }
  }
}
