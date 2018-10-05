// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name InheritedInitializerBase %S/Inputs/inherited-initializer-base.swift
// RUN: %target-swift-frontend -emit-silgen -I %t %s | %FileCheck %s

import InheritedInitializerBase

class InheritsInit : Base {}

// CHECK-LABEL: sil hidden @$s4main10testSimpleyyF
func testSimple() {
  // CHECK: [[DEFAULT:%.+]] = function_ref @$s24InheritedInitializerBase0C0CyACSicfcfA_
  // CHECK: [[ARG:%.+]] = apply [[DEFAULT]]()
  // CHECK: [[INIT:%.+]] = function_ref @$s4main12InheritsInitCyACSicfC
  // CHECK: apply [[INIT]]([[ARG]], {{%.+}})
  _ = InheritsInit()

  // CHECK: [[VALUE:%.+]] = integer_literal $Builtin.Int2048, 5
  // CHECK: [[ARG:%.+]] = apply {{%.+}}([[VALUE]], {{%.+}}) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
  // CHECK: [[INIT:%.+]] = function_ref @$s4main12InheritsInitCyACSicfC
  // CHECK: apply [[INIT]]([[ARG]], {{%.+}})
  _ = InheritsInit(5)
} // CHECK: end sil function '$s4main10testSimpleyyF'

struct Reinitializable<T>: Initializable {
  init() {}
}

class GenericSub<T: Initializable> : GenericBase<T> {}
class ModifiedGenericSub<U> : GenericBase<Reinitializable<U>> {}
class NonGenericSub : GenericBase<Reinitializable<Int>> {}

// CHECK-LABEL: sil hidden @$s4main11testGenericyyF
func testGeneric() {
  // CHECK: [[TYPE:%.+]] = metatype $@thick GenericSub<Reinitializable<Int8>>.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @$s24InheritedInitializerBase07GenericC0CyACyxGxcfcfA_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int8>>({{%.+}})
  // CHECK: [[INIT:%.+]] = function_ref @$s4main10GenericSubCyACyxGxcfC
  // CHECK: apply [[INIT]]<Reinitializable<Int8>>({{%.+}}, [[TYPE]])
  _ = GenericSub<Reinitializable<Int8>>.init() // works around SR-3806

  // CHECK: [[TYPE:%.+]] = metatype $@thick ModifiedGenericSub<Int16>.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @$s24InheritedInitializerBase07GenericC0CyACyxGxcfcfA_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int16>>({{%.+}})
  // CHECK: [[INIT:%.+]] = function_ref @$s4main18ModifiedGenericSubCyACyxGAA15ReinitializableVyxGcfC
  // CHECK: apply [[INIT]]<Int16>({{%.+}}, [[TYPE]])
  _ = ModifiedGenericSub<Int16>()

  // CHECK: [[TYPE:%.+]] = metatype $@thick NonGenericSub.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @$s24InheritedInitializerBase07GenericC0CyACyxGxcfcfA_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int>>({{%.+}})
  // CHECK: [[INIT:%.+]] = function_ref @$s4main13NonGenericSubCyAcA15ReinitializableVySiGcfC
  // CHECK: apply [[INIT]]({{%.+}}, [[TYPE]])
  _ = NonGenericSub()
} // CHECK: end sil function '$s4main11testGenericyyF'
