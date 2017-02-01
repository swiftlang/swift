// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name InheritedInitializerBase %S/Inputs/inherited-initializer-base.swift
// RUN: %target-swift-frontend -emit-silgen -I %t %s | %FileCheck %s

import InheritedInitializerBase

class InheritsInit : Base {}

// CHECK-LABEL: sil hidden @_TF4main10testSimpleFT_T_
func testSimple() {
  // CHECK: [[INIT:%.+]] = function_ref @_TFC4main12InheritsInitCfSiS0_
  // CHECK: [[DEFAULT:%.+]] = function_ref @_TIFC24InheritedInitializerBase4BasecFSiS0_A_
  // CHECK: [[ARG:%.+]] = apply [[DEFAULT]]()
  // CHECK: apply [[INIT]]([[ARG]], {{%.+}})
  _ = InheritsInit()

  // CHECK: [[INIT:%.+]] = function_ref @_TFC4main12InheritsInitCfSiS0_
  // CHECK: [[VALUE:%.+]] = integer_literal $Builtin.Int2048, 5
  // CHECK: [[ARG:%.+]] = apply {{%.+}}([[VALUE]], {{%.+}}) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
  // CHECK: apply [[INIT]]([[ARG]], {{%.+}})
  _ = InheritsInit(5)
} // CHECK: end sil function '_TF4main10testSimpleFT_T_'

struct Reinitializable<T>: Initializable {
  init() {}
}

class GenericSub<T: Initializable> : GenericBase<T> {}
class ModifiedGenericSub<U> : GenericBase<Reinitializable<U>> {}
class NonGenericSub : GenericBase<Reinitializable<Int>> {}

// CHECK-LABEL: sil hidden @_TF4main11testGenericFT_T_
func testGeneric() {
  // CHECK: [[INIT:%.+]] = function_ref @_TFC4main10GenericSubCfxGS0_x_
  // CHECK: [[TYPE:%.+]] = metatype $@thick GenericSub<Reinitializable<Int8>>.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @_TIFC24InheritedInitializerBase11GenericBasecFxGS0_x_A_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int8>>({{%.+}})
  // CHECK: apply [[INIT]]<Reinitializable<Int8>>({{%.+}}, [[TYPE]])
  _ = GenericSub<Reinitializable<Int8>>.init() // works around SR-3806

  // CHECK: [[INIT:%.+]] = function_ref @_TFC4main18ModifiedGenericSubCfGVS_15Reinitializablex_GS0_x_
  // CHECK: [[TYPE:%.+]] = metatype $@thick ModifiedGenericSub<Int16>.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @_TIFC24InheritedInitializerBase11GenericBasecFxGS0_x_A_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int16>>({{%.+}})
  // CHECK: apply [[INIT]]<Int16>({{%.+}}, [[TYPE]])
  _ = ModifiedGenericSub<Int16>()

  // CHECK: [[INIT:%.+]] = function_ref @_TFC4main13NonGenericSubCfGVS_15ReinitializableSi_S0_
  // CHECK: [[TYPE:%.+]] = metatype $@thick NonGenericSub.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @_TIFC24InheritedInitializerBase11GenericBasecFxGS0_x_A_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int>>({{%.+}})
  // CHECK: apply [[INIT]]({{%.+}}, [[TYPE]])
  _ = NonGenericSub()
} // CHECK: end sil function '_TF4main11testGenericFT_T_'