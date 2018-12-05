// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -I %S/../IDE/Inputs/custom-modules %s -enable-sil-ownership | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import ImportAsMember.Class

// CHECK-LABEL: sil shared [serializable] [thunk] @$sSo4HiveC5queenABSgSo3BeeCSg_tcfCTO : $@convention(method) (@owned Optional<Bee>, @thick Hive.Type) -> @owned Optional<Hive>
func testInstanceTypeFactoryMethod(queen: Bee) {
  // CHECK: bb0([[QUEEN:%[0-9]+]] : @owned $Optional<Bee>, [[HIVE_META:%[0-9]+]] : $@thick Hive.Type):
  // CHECK-NEXT:   [[HIVE_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[HIVE_META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
  // CHECK-NEXT:   [[FACTORY:%[0-9]+]] = objc_method [[HIVE_META_OBJC]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign : (Hive.Type) -> (Bee?) -> Hive?, $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK-NEXT:   [[HIVE:%[0-9]+]] = apply [[FACTORY]]([[QUEEN]], [[HIVE_META_OBJC]]) : $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK-NEXT:   destroy_value [[QUEEN]]
  // CHECK-NEXT:   return [[HIVE]] : $Optional<Hive>
  var hive1 = Hive(queen: queen)
}

extension Hive {
  // FIXME: This whole approach is wrong. This should be a factory initializer,
  // not a convenience initializer, which means it does not have an initializing
  // entry point at all.

  // CHECK-LABEL: sil hidden @$sSo4HiveC17objc_factory_initE10otherQueenABSo3BeeC_tcfC
  // CHECK: bb0([[QUEEN:%.*]] : @owned $Bee, [[META:%.*]] : $@thick Hive.Type):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var Hive }, let, name "self"
  // CHECK:   [[MU:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MU]] : ${ var Hive }, 0
  // CHECK:   [[OBJC_META:%[0-9]+]] = thick_to_objc_metatype [[META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
  // CHECK:   [[BORROWED_QUEEN:%.*]] = begin_borrow [[QUEEN]]
  // CHECK:   [[COPIED_BORROWED_QUEEN:%.*]] = copy_value [[BORROWED_QUEEN]]
  // CHECK:   [[OPT_COPIED_BORROWED_QUEEN:%.*]] = enum $Optional<Bee>, #Optional.some!enumelt.1, [[COPIED_BORROWED_QUEEN]]
  // CHECK:   [[FACTORY:%[0-9]+]] = objc_method [[OBJC_META]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign : (Hive.Type) -> (Bee?) -> Hive?, $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK:   [[NEW_HIVE:%.*]] = apply [[FACTORY]]([[OPT_COPIED_BORROWED_QUEEN]], [[OBJC_META]]) : $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK:   destroy_value [[OPT_COPIED_BORROWED_QUEEN]]
  // CHECK:   end_borrow [[BORROWED_QUEEN]]
  // CHECK:   switch_enum [[NEW_HIVE]] : $Optional<Hive>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[NEW_HIVE:%.*]] : @owned $Hive):
  // CHECK:   assign [[NEW_HIVE]] to [[PB_BOX]]
  // CHECK: } // end sil function '$sSo4HiveC17objc_factory_initE10otherQueenABSo3BeeC_tcfC'
  convenience init(otherQueen other: Bee) {
    self.init(queen: other)
  }
}

extension SomeClass {
  // CHECK-LABEL: sil hidden @$sSo12IAMSomeClassC17objc_factory_initE6doubleABSd_tcfC
  // CHECK: bb0([[DOUBLE:%.*]] : $Double,
  // CHECK-NOT: value_metatype
  // CHECK: [[FNREF:%[0-9]+]] = function_ref @MakeIAMSomeClass
  // CHECK: apply [[FNREF]]([[DOUBLE]])
  convenience init(double: Double) {
    self.init(value: double)
  }
}

class SubHive : Hive {
  // CHECK-LABEL: sil hidden @$s17objc_factory_init7SubHiveC20delegatesToInheritedACyt_tcfC
  // CHECK: bb0([[METATYPE:%.*]] : $@thick SubHive.Type):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var SubHive }, let, name "self"
  // CHECK:   [[MU:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]] : ${ var SubHive }
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MU]] : ${ var SubHive }, 0
  // CHECK:   [[UP_METATYPE:%.*]] = upcast [[METATYPE]]
  // CHECK:   [[OBJC_METATYPE:%.*]] = thick_to_objc_metatype [[UP_METATYPE]]
  // CHECK:   [[QUEEN:%.*]] = unchecked_ref_cast {{.*}} : $Bee to $Optional<Bee>
  // CHECK:   [[HIVE_INIT_FN:%.*]] = objc_method [[OBJC_METATYPE]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign
  // CHECK:   apply [[HIVE_INIT_FN]]([[QUEEN]], [[OBJC_METATYPE]]) : $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK:   destroy_value [[QUEEN]]
  // CHECK: } // end sil function '$s17objc_factory_init7SubHiveC20delegatesToInheritedACyt_tcfC'
  convenience init(delegatesToInherited: ()) {
    self.init(queen: Bee())
  }
}
