// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name InheritedInitializerBase %S/Inputs/inherited-initializer-base.swift
// RUN: %target-swift-frontend -emit-silgen -I %t %s | %FileCheck %s

import InheritedInitializerBase

class InheritsInit : Base {}

// CHECK-LABEL: sil hidden @_T04main10testSimpleyyF
func testSimple() {
  // CHECK: [[INIT:%.+]] = function_ref @_T04main12InheritsInitCACSicfC
  // CHECK: [[DEFAULT:%.+]] = function_ref @_T024InheritedInitializerBase0C0CACSicfcfA_
  // CHECK: [[ARG:%.+]] = apply [[DEFAULT]]()
  // CHECK: apply [[INIT]]([[ARG]], {{%.+}})
  _ = InheritsInit()

  // CHECK: [[INIT:%.+]] = function_ref @_T04main12InheritsInitCACSicfC
  // CHECK: [[VALUE:%.+]] = integer_literal $Builtin.Int2048, 5
  // CHECK: [[ARG:%.+]] = apply {{%.+}}([[VALUE]], {{%.+}}) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
  // CHECK: apply [[INIT]]([[ARG]], {{%.+}})
  _ = InheritsInit(5)
} // CHECK: end sil function '_T04main10testSimpleyyF'

struct Reinitializable<T>: Initializable {
  init() {}
}

class GenericSub<T: Initializable> : GenericBase<T> {}
class ModifiedGenericSub<U> : GenericBase<Reinitializable<U>> {}
class NonGenericSub : GenericBase<Reinitializable<Int>> {}

// CHECK-LABEL: sil hidden @_T04main11testGenericyyF
func testGeneric() {
  // CHECK: [[INIT:%.+]] = function_ref @_T04main10GenericSubCACyxGxcfC
  // CHECK: [[TYPE:%.+]] = metatype $@thick GenericSub<Reinitializable<Int8>>.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @_T024InheritedInitializerBase07GenericC0CACyxGxcfcfA_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int8>>({{%.+}})
  // CHECK: apply [[INIT]]<Reinitializable<Int8>>({{%.+}}, [[TYPE]])
  _ = GenericSub<Reinitializable<Int8>>.init() // works around SR-3806

  // CHECK: [[INIT:%.+]] = function_ref @_T04main18ModifiedGenericSubCACyxGAA15ReinitializableVyxGcfC
  // CHECK: [[TYPE:%.+]] = metatype $@thick ModifiedGenericSub<Int16>.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @_T024InheritedInitializerBase07GenericC0CACyxGxcfcfA_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int16>>({{%.+}})
  // CHECK: apply [[INIT]]<Int16>({{%.+}}, [[TYPE]])
  _ = ModifiedGenericSub<Int16>()

  // CHECK: [[INIT:%.+]] = function_ref @_T04main13NonGenericSubCAcA15ReinitializableVySiGcfC
  // CHECK: [[TYPE:%.+]] = metatype $@thick NonGenericSub.Type
  // CHECK: [[DEFAULT:%.+]] = function_ref @_T024InheritedInitializerBase07GenericC0CACyxGxcfcfA_
  // CHECK: apply [[DEFAULT]]<Reinitializable<Int>>({{%.+}})
  // CHECK: apply [[INIT]]({{%.+}}, [[TYPE]])
  _ = NonGenericSub()
} // CHECK: end sil function '_T04main11testGenericyyF'