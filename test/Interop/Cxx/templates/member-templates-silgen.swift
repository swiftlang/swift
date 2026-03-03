// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import MemberTemplates

// CHECK-LABEL: sil hidden @$s4main9basicTestyyF : $@convention(thin) () -> ()

// CHECK: [[ADD:%.*]] = function_ref @$sSo18HasMemberTemplatesV17addSameTypeParamsys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32
// CHECK: apply [[ADD]]({{.*}}) : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK: [[ADD_TWO_TEMPLATES:%.*]] = function_ref @$sSo18HasMemberTemplatesV18addMixedTypeParamsys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32
// CHECK: apply [[ADD_TWO_TEMPLATES]]({{.*}}) : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK: [[ADD_ALL:%.*]] = function_ref @$sSo18HasMemberTemplatesV6addAllys5Int32VAE_A2EtFTo : $@convention(cxx_method) (Int32, Int32, Int32, @inout HasMemberTemplates) -> Int32
// CHECK: apply [[ADD_ALL]]({{.*}}) : $@convention(cxx_method) (Int32, Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK: [[DO_NOTHING:%.*]] = function_ref @$sSo18HasMemberTemplatesV17doNothingConstRefyys5Int32VFTo : $@convention(cxx_method) (@in_guaranteed Int32, @inout HasMemberTemplates) -> ()
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

// CHECK-LABEL: sil [asmname "{{.*}}addSameTypeParams{{.*}}"] [clang HasMemberTemplates.addSameTypeParams] @$sSo18HasMemberTemplatesV17addSameTypeParamsys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK-LABEL: sil [asmname "{{.*}}addMixedTypeParams{{.*}}"] [clang HasMemberTemplates.addMixedTypeParams] @$sSo18HasMemberTemplatesV18addMixedTypeParamsys5Int32VAE_AEtFTo : $@convention(cxx_method) (Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK-LABEL: sil [asmname "{{.*}}addAll{{.*}}"] [clang HasMemberTemplates.addAll] @$sSo18HasMemberTemplatesV6addAllys5Int32VAE_A2EtFTo : $@convention(cxx_method) (Int32, Int32, Int32, @inout HasMemberTemplates) -> Int32

// CHECK-LABEL: sil [asmname "{{.*}}doNothingConstRef{{.*}}"] [clang HasMemberTemplates.doNothingConstRef] @$sSo18HasMemberTemplatesV17doNothingConstRefyys5Int32VFTo : $@convention(cxx_method) (@in_guaranteed Int32, @inout HasMemberTemplates) -> ()

// CHECK-LABEL: sil hidden @$s4main12testSetValueyyF : $@convention(thin) () -> ()

// CHECK: [[SET_VALUE:%.*]] = function_ref @$sSo0044TemplateClassWithMemberTemplatesCInt_CeCInkcV8setValueyySiFTo : $@convention(cxx_method) (Int, @inout TemplateClassWithMemberTemplates<CInt>) -> ()
// CHECK: apply [[SET_VALUE]]({{.*}}) : $@convention(cxx_method) (Int, @inout TemplateClassWithMemberTemplates<CInt>) -> ()

// CHECK-LABEL: end sil function '$s4main12testSetValueyyF'
func testSetValue() {
  var w = IntWrapper(11)
  w.setValue(42)
}

// CHECK-LABEL: sil hidden @$s4main17testStaticMembersyyF : $@convention(thin) () -> ()

// CHECK: [[ADD_FN:%.*]] = function_ref @$sSo24HasStaticMemberTemplatesV3addyS2i_SitFZTo : $@convention(c) (Int, Int) -> Int
// CHECK: apply [[ADD_FN]]({{.*}}) : $@convention(c) (Int, Int) -> Int

// CHECK: [[ADD_TWO_TEMPLATES_FN:%.*]] = function_ref @$sSo24HasStaticMemberTemplatesV06addTwoD0yS2i_s4Int8VtFZTo : $@convention(c) (Int, Int8) -> Int
// CHECK: apply [[ADD_TWO_TEMPLATES_FN]]({{.*}}) : $@convention(c) (Int, Int8) -> Int

// CHECK: [[REMOVE_REFERENCE_FN:%.*]] = function_ref @$sSo24HasStaticMemberTemplatesV15removeReferenceyS2izFZTo : $@convention(c) (@inout Int) -> Int
// CHECK: apply [[REMOVE_REFERENCE_FN]]({{.*}}) : $@convention(c) (@inout Int) -> Int

// CHECK-LABEL: end sil function '$s4main17testStaticMembersyyF'
func testStaticMembers() {
  var x: Int = 0
  let y: CChar = 0
  HasStaticMemberTemplates.add(x, x)
  HasStaticMemberTemplates.addTwoTemplates(x, y)
  HasStaticMemberTemplates.removeReference(&x)
}

// CHECK: sil hidden_external [asmname "{{.*}}add{{.*}}"] [clang HasStaticMemberTemplates.add] @$sSo24HasStaticMemberTemplatesV3addyS2i_SitFZTo : $@convention(c) (Int, Int) -> Int

// CHECK: sil hidden_external [asmname "{{.*}}addTwoTemplates{{.*}}"] [clang HasStaticMemberTemplates.addTwoTemplates] @$sSo24HasStaticMemberTemplatesV06addTwoD0yS2i_s4Int8VtFZTo : $@convention(c) (Int, Int8) -> Int

// CHECK: sil hidden_external [asmname "{{.*}}removeReference{{.*}}"] [clang HasStaticMemberTemplates.removeReference] @$sSo24HasStaticMemberTemplatesV15removeReferenceyS2izFZTo : $@convention(c) (@inout Int) -> Int
