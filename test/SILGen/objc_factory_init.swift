// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -I %S/../IDE/Inputs/custom-modules %s -enable-sil-ownership | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import ImportAsMember.Class

// CHECK-LABEL: sil shared [serializable] [thunk] @$SSo4HiveC5queenABSgSo3BeeCSg_tcfCTO : $@convention(method) (@owned Optional<Bee>, @thick Hive.Type) -> @owned Optional<Hive>
func testInstanceTypeFactoryMethod(queen: Bee) {
  // CHECK: bb0([[QUEEN:%[0-9]+]] : @owned $Optional<Bee>, [[HIVE_META:%[0-9]+]] : @trivial $@thick Hive.Type):
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

  // CHECK-LABEL: sil hidden @$SSo4HiveC17objc_factory_initE10otherQueenABSo3BeeC_tcfc : $@convention(method) (@owned Bee, @owned Hive) -> @owned Hive {
  // CHECK: bb0([[QUEEN:%.*]] : @owned $Bee, [[OLD_HIVE:%.*]] : @owned $Hive):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var Hive }, let, name "self"
  // CHECK:   [[MU:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MU]] : ${ var Hive }, 0
  // CHECK:   store [[OLD_HIVE]] to [init] [[PB_BOX]]
  // CHECK:   [[BORROWED_SELF:%.*]] = load_borrow [[PB_BOX]]
  // CHECK:   [[META:%[0-9]+]] = value_metatype $@thick Hive.Type, [[BORROWED_SELF]] : $Hive
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[PB_BOX]]
  // CHECK:   [[OBJC_META:%[0-9]+]] = thick_to_objc_metatype [[META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
  // CHECK:   [[BORROWED_QUEEN:%.*]] = begin_borrow [[QUEEN]]
  // CHECK:   [[COPIED_BORROWED_QUEEN:%.*]] = copy_value [[BORROWED_QUEEN]]
  // CHECK:   [[OPT_COPIED_BORROWED_QUEEN:%.*]] = enum $Optional<Bee>, #Optional.some!enumelt.1, [[COPIED_BORROWED_QUEEN]]
  // CHECK:   [[FACTORY:%[0-9]+]] = objc_method [[OBJC_META]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign : (Hive.Type) -> (Bee?) -> Hive?, $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK:   [[NEW_HIVE:%.*]] = apply [[FACTORY]]([[OPT_COPIED_BORROWED_QUEEN]], [[OBJC_META]]) : $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK:   destroy_value [[OPT_COPIED_BORROWED_QUEEN]]
  // CHECK:   end_borrow [[BORROWED_QUEEN]] from [[QUEEN]]
  // CHECK:   switch_enum [[NEW_HIVE]] : $Optional<Hive>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[NEW_HIVE:%.*]] : @owned $Hive):
  // CHECK:   assign [[NEW_HIVE]] to [[PB_BOX]]
  // CHECK: } // end sil function '$SSo4HiveC17objc_factory_initE10otherQueenABSo3BeeC_tcfc'
  convenience init(otherQueen other: Bee) {
    self.init(queen: other)
  }

  // CHECK-LABEL: sil hidden @$SSo4HiveC17objc_factory_initE15otherFlakyQueenABSo3BeeC_tKcfC : $@convention(method) (@owned Bee, @thick Hive.Type) -> (@owned Hive, @error Error) {
  // CHECK: bb0([[QUEEN:%.*]] : @owned $Bee, [[METATYPE:%.*]] : @trivial $@thick Hive.Type):
  // CHECK:   [[OBJC_METATYPE:%.*]] = thick_to_objc_metatype [[METATYPE]]
  // CHECK:   [[HIVE:%.*]] = alloc_ref_dynamic [objc] [[OBJC_METATYPE]]
  // CHECK:   try_apply {{.*}}([[QUEEN]], [[HIVE]]) : $@convention(method) (@owned Bee, @owned Hive) -> (@owned Hive, @error Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // CHECK: [[NORMAL_BB]]([[HIVE:%.*]] : @owned $Hive):
  // CHECK:   return [[HIVE]]
  //
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK:   builtin "willThrow"([[ERROR]] : $Error)
  // CHECK:   throw [[ERROR]]
  // CHECK: } // end sil function '$SSo4HiveC17objc_factory_initE15otherFlakyQueenABSo3BeeC_tKcfC'
  convenience init(otherFlakyQueen other: Bee) throws {
    try self.init(flakyQueen: other)
  }
}

extension SomeClass {
  // CHECK-LABEL: sil hidden @$SSo12IAMSomeClassC17objc_factory_initE6doubleABSd_tcfc : $@convention(method) (Double, @owned SomeClass) -> @owned SomeClass {
  // CHECK: bb0([[DOUBLE:%.*]] : @trivial $Double,
  // CHECK-NOT: value_metatype
  // CHECK: [[FNREF:%[0-9]+]] = function_ref @MakeIAMSomeClass
  // CHECK: apply [[FNREF]]([[DOUBLE]])
  // CHECK: } // end sil function '$SSo12IAMSomeClassC17objc_factory_initE6doubleABSd_tcfc'
  convenience init(double: Double) {
    self.init(value: double)
  }
}

class SubHive : Hive {
  // CHECK-LABEL: sil hidden @$S17objc_factory_init7SubHiveC20delegatesToInheritedACyt_tcfc : $@convention(method) (@owned SubHive) -> @owned SubHive {
  // CHECK: bb0([[SUBHIVE:%.*]] : @owned $SubHive):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var SubHive }, let, name "self"
  // CHECK:   [[MU:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]] : ${ var SubHive }
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MU]] : ${ var SubHive }, 0
  // CHECK:   store [[SUBHIVE]] to [init] [[PB_BOX]]
  // CHECK:   [[BORROWED_SELF:%.*]] = load_borrow [[PB_BOX]]
  // CHECK:   [[UPCAST_BORROWED_SELF:%.*]] = upcast [[BORROWED_SELF]] : $SubHive to $Hive
  // CHECK:   [[METATYPE:%.*]] = value_metatype $@thick Hive.Type, [[UPCAST_BORROWED_SELF:%.*]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[PB_BOX]]
  // CHECK:   [[OBJC_METATYPE:%.*]] = thick_to_objc_metatype [[METATYPE]]
  // CHECK:   [[QUEEN:%.*]] = unchecked_ref_cast {{.*}} : $Bee to $Optional<Bee>
  // CHECK:   [[HIVE_INIT_FN:%.*]] = objc_method [[OBJC_METATYPE]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign
  // CHECK:   apply [[HIVE_INIT_FN]]([[QUEEN]], [[OBJC_METATYPE]]) : $@convention(objc_method) (Optional<Bee>, @objc_metatype Hive.Type) -> @autoreleased Optional<Hive>
  // CHECK:   destroy_value [[QUEEN]]
  // CHECK: } // end sil function '$S17objc_factory_init7SubHiveC20delegatesToInheritedACyt_tcfc'
  convenience init(delegatesToInherited: ()) {
    self.init(queen: Bee())
  }
}
