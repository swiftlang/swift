// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import MemberTemplates

// CHECK-LABEL: sil hidden @$s4main9basicTestyyF : $@convention(thin) () -> ()

// CHECK: [[ADD:%.*]] = function_ref @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18ab37Templates17addSameTypeParamsIiEET_S1_N1_ys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32
// CHECK: apply [[ADD]]({{.*}}) : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK: [[ADD_TWO_TEMPLATES:%.*]] = function_ref @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18aB42Templates18addMixedTypeParamsIiiEET_S1_T0_ys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32
// CHECK: apply [[ADD_TWO_TEMPLATES]]({{.*}}) : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK: [[ADD_ALL:%.*]] = function_ref @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18aB28Templates6addAllIiiEEiiT_T0_ys5Int32VAE_A2EtFTo : $@convention(cxx_method) (Int32, Int32, Int32, @inout HasMemberTemplates) -> Int32
// CHECK: apply [[ADD_ALL]]({{.*}}) : $@convention(cxx_method) (Int32, Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK: [[DO_NOTHING:%.*]] = function_ref @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18aB37Templates17doNothingConstRefIiEEvRKT_yys5Int32VFTo : $@convention(cxx_method) (@in_guaranteed Int32, @inout HasMemberTemplates) -> ()
// CHECK: apply [[DO_NOTHING]]({{.*}}) : $@convention(cxx_method) (@in_guaranteed Int32, @inout HasMemberTemplates) -> ()

// CHECK-LABEL: end sil function '$s4main9basicTestyyF'
func basicTest() {
  var i: Int32 = 0
  var obj = HasMemberTemplates()
  obj.addSameTypeParams(i, i)
  obj.addMixedTypeParams(i, i)
  obj.addAll(i, i, i)
  obj.doNothingConstRef(i)
}

// CHECK-LABEL: sil [asmname "{{.*}}addSameTypeParams{{.*}}"] [clang HasMemberTemplates.__swift_specializedThunk__ZN18HasMemberTemplates17addSameTypeParamsIiEET_S1_S1_] @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18ab37Templates17addSameTypeParamsIiEET_S1_N1_ys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK-LABEL: sil [asmname "{{.*}}addMixedTypeParams{{.*}}"] [clang HasMemberTemplates.__swift_specializedThunk__ZN18HasMemberTemplates18addMixedTypeParamsIiiEET_S1_T0_] @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18aB42Templates18addMixedTypeParamsIiiEET_S1_T0_ys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK-LABEL: sil [asmname "{{.*}}addAll{{.*}}"] [clang HasMemberTemplates.__swift_specializedThunk__ZN18HasMemberTemplates6addAllIiiEEiiT_T0_] @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18aB28Templates6addAllIiiEEiiT_T0_ys5Int32VAE_A2EtFTo : $@convention(cxx_method) (Int32, Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK-LABEL: sil [asmname "{{.*}}doNothingConstRef{{.*}}"] [clang HasMemberTemplates.__swift_specializedThunk__ZN18HasMemberTemplates17doNothingConstRefIiEEvRKT_] @$sSo18HasMemberTemplatesV030__swift_specializedThunk__ZN18aB37Templates17doNothingConstRefIiEEvRKT_yys5Int32VFTo : $@convention(cxx_method) (@in_guaranteed Int32, @inout HasMemberTemplates) -> ()

// CHECK-LABEL: sil hidden @$s4main12testSetValueyyF : $@convention(thin) () -> ()

// CHECK: [[SET_VALUE:%.*]] = function_ref @$sSo0044TemplateClassWithMemberTemplatesCInt_CeCInkcV81__swift_specializedThunk__ZN32TemplateClassWithMemberTemplatesIiE8setValueIlEEvT_yySiFTo : $@convention(cxx_method) (Int, @inout TemplateClassWithMemberTemplates<CInt>) -> ()
// CHECK: apply [[SET_VALUE]]({{.*}}) : $@convention(cxx_method) (Int, @inout TemplateClassWithMemberTemplates<CInt>) -> ()

// CHECK-LABEL: end sil function '$s4main12testSetValueyyF'
func testSetValue() {
  var w = IntWrapper(11)
  w.setValue(42)
}

// CHECK-LABEL: sil hidden @$s4main17testStaticMembersyyF : $@convention(thin) () -> ()

// CHECK: [[ADD_FN:%.*]] = function_ref @$sSo24HasStaticMemberTemplatesV030__swift_specializedThunk__ZN24abc22Templates3addIlEET_S1_L1_yS2i_SitFZTo : $@convention(c) (Int, Int) -> Int
// CHECK: apply [[ADD_FN]]({{.*}}) : $@convention(c) (Int, Int) -> Int

// CHECK: [[ADD_TWO_TEMPLATES_FN:%.*]] = function_ref @$sSo24HasStaticMemberTemplatesV030__swift_specializedThunk__ZN24abc17Templates15addTwoD13IlcEET_S1_T0_yS2i_s4Int8VtFZTo : $@convention(c) (Int, Int8) -> Int
// CHECK: apply [[ADD_TWO_TEMPLATES_FN]]({{.*}}) : $@convention(c) (Int, Int8) -> Int

// CHECK: [[REMOVE_REFERENCE_FN:%.*]] = function_ref @$sSo24HasStaticMemberTemplatesV030__swift_specializedThunk__ZN24abC36Templates15removeReferenceIlEET_RS1_yS2izFZTo : $@convention(c) (@inout Int) -> Int
// CHECK: apply [[REMOVE_REFERENCE_FN]]({{.*}}) : $@convention(c) (@inout Int) -> Int

// CHECK-LABEL: end sil function '$s4main17testStaticMembersyyF'
func testStaticMembers() {
  var x: Int = 0
  let y: CChar = 0
  HasStaticMemberTemplates.add(x, x)
  HasStaticMemberTemplates.addTwoTemplates(x, y)
  HasStaticMemberTemplates.removeReference(&x)
}

// CHECK: sil hidden_external [asmname "{{.*}}add{{.*}}"] [clang HasStaticMemberTemplates.__swift_specializedThunk__ZN24HasStaticMemberTemplates3addIlEET_S1_S1_] @$sSo24HasStaticMemberTemplatesV030__swift_specializedThunk__ZN24abc22Templates3addIlEET_S1_L1_yS2i_SitFZTo : $@convention(c) (Int, Int) -> Int

// CHECK: sil hidden_external [asmname "{{.*}}addTwoTemplates{{.*}}"] [clang HasStaticMemberTemplates.__swift_specializedThunk__ZN24HasStaticMemberTemplates15addTwoTemplatesIlcEET_S1_T0_] @$sSo24HasStaticMemberTemplatesV030__swift_specializedThunk__ZN24abc17Templates15addTwoD13IlcEET_S1_T0_yS2i_s4Int8VtFZTo : $@convention(c) (Int, Int8) -> Int

// CHECK: sil hidden_external [asmname "{{.*}}removeReference{{.*}}"] [clang HasStaticMemberTemplates.__swift_specializedThunk__ZN24HasStaticMemberTemplates15removeReferenceIlEET_RS1_] @$sSo24HasStaticMemberTemplatesV030__swift_specializedThunk__ZN24abC36Templates15removeReferenceIlEET_RS1_yS2izFZTo : $@convention(c) (@inout Int) -> Int
