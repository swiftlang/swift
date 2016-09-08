// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/../IDE/Inputs/custom-modules %s -emit-sil | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import ImportAsMember.Class

// CHECK-LABEL: sil shared [thunk] @_TTOFCSo4HiveCfT5queenGSQCSo3Bee__GSQS__ : $@convention(method) (@owned ImplicitlyUnwrappedOptional<Bee>, @thick Hive.Type) -> @owned ImplicitlyUnwrappedOptional<Hive>
func testInstanceTypeFactoryMethod(queen: Bee) {
  // CHECK: bb0([[QUEEN:%[0-9]+]] : $ImplicitlyUnwrappedOptional<Bee>, [[HIVE_META:%[0-9]+]] : $@thick Hive.Type):
  // CHECK-NEXT:   [[HIVE_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[HIVE_META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
  // CHECK-NEXT:   [[FACTORY:%[0-9]+]] = class_method [volatile] [[HIVE_META_OBJC]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign : (Hive.Type) -> (Bee!) -> Hive! , $@convention(objc_method) (ImplicitlyUnwrappedOptional<Bee>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
  // CHECK-NEXT:   [[HIVE:%[0-9]+]] = apply [[FACTORY]]([[QUEEN]], [[HIVE_META_OBJC]]) : $@convention(objc_method) (ImplicitlyUnwrappedOptional<Bee>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
  // CHECK-NEXT:   release_value [[QUEEN]]
  // CHECK-NEXT:   return [[HIVE]] : $ImplicitlyUnwrappedOptional<Hive>
  var hive1 = Hive(queen: queen)
}

extension Hive {
  // FIXME: This whole approach is wrong. This should be a factory
  // initializer, not a convenience initializer, which means it does
  // not have an initializing entry point at all.

  // CHECK-LABEL: sil hidden @_TFE31definite_init_objc_factory_initCSo4HivecfT10otherQueenCSo3Bee_S0_ : $@convention(method) (@owned Bee, @owned Hive) -> @owned Hive
  convenience init(otherQueen other: Bee) {
    // CHECK: [[SELF_ADDR:%[0-9]+]] = alloc_stack $Hive
    // CHECK: store [[OLD_SELF:%[0-9]+]] to [[SELF_ADDR]]
    // CHECK: [[META:%[0-9]+]] = value_metatype $@thick Hive.Type, [[OLD_SELF]] : $Hive
    // CHECK: [[FACTORY:%[0-9]+]] = class_method [volatile] [[META]] : $@thick Hive.Type, #Hive.init!allocator.1.foreign : (Hive.Type) -> (Bee!) -> Hive! , $@convention(objc_method) (ImplicitlyUnwrappedOptional<Bee>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
    // CHECK: [[OBJC_META:%[0-9]+]] = thick_to_objc_metatype [[META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
    // CHECK: apply [[FACTORY]]([[QUEEN:%[0-9]+]], [[OBJC_META]]) : $@convention(objc_method) (ImplicitlyUnwrappedOptional<Bee>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
    // CHECK: store [[NEW_SELF:%[0-9]+]] to [[SELF_ADDR]]
    // CHECK: [[METATYPE:%.*]] = value_metatype $@thick Hive.Type, [[OLD_SELF]] : $Hive
    // CHECK: dealloc_partial_ref [[OLD_SELF]] : $Hive, [[METATYPE]] : $@thick Hive.Type
    // CHECK: dealloc_stack [[SELF_ADDR]]
    // CHECK: return [[NEW_SELF]]
    self.init(queen: other)
  }

  convenience init(otherFlakyQueen other: Bee) throws {
    try self.init(flakyQueen: other)
  }
}

extension SomeClass {
  // SIL-LABEL: sil hidden @_TFE16import_as_memberCSo9SomeClasscfT6doubleSd_S0_
  // SIL: bb0([[DOUBLE:%[0-9]+]] : $Double
  // SIL-NOT: value_metatype
  // SIL: [[FNREF:%[0-9]+]] = function_ref @MakeIAMSomeClass
  // SIL: apply [[FNREF]]([[DOUBLE]])
  convenience init(double: Double) {
    self.init(value: double)
  }
}
