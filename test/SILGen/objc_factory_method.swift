// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

// FIXME: The -emit-sil line is there only to check that the generated
// SIL doesn't freak out DI.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -emit-sil | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil shared @_TTOFCSo4HiveCfMS_FT5queenGSQCSo1B__GSQS__ : $@thin (@owned ImplicitlyUnwrappedOptional<B>, @thick Hive.Type) -> @owned ImplicitlyUnwrappedOptional<Hive>
func testInstanceTypeFactoryMethod(queen: B) {
  // CHECK-NEXT: bb0([[QUEEN:%[0-9]+]] : $ImplicitlyUnwrappedOptional<B>, [[HIVE_META:%[0-9]+]] : $@thick Hive.Type):
  // CHECK-NEXT:   [[HIVE_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[HIVE_META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
  // CHECK-NEXT:   [[FACTORY:%[0-9]+]] = class_method [volatile] [[HIVE_META_OBJC]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign : Hive.Type -> (queen: B!) -> Hive! , $@cc(objc_method) @thin (ImplicitlyUnwrappedOptional<B>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
  // CHECK-NEXT:   [[HIVE:%[0-9]+]] = apply [[FACTORY]]([[QUEEN]], [[HIVE_META_OBJC]]) : $@cc(objc_method) @thin (ImplicitlyUnwrappedOptional<B>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
// CHECK-NEXT:   strong_retain_autoreleased [[HIVE]] : $ImplicitlyUnwrappedOptional<Hive>
// CHECK-NEXT:   release_value [[QUEEN]]
// CHECK-NEXT:   return [[HIVE]] : $ImplicitlyUnwrappedOptional<Hive>
  var hive1 = Hive(queen: queen)
}

extension Hive {
  // FIXME: This whole approach is wrong. This should be a factory
  // initializer, not a convenience initializer, which means it does
  // not have an initializing entry point at all.
  // CHECK-LABEL: sil hidden @_TFE19objc_factory_methodCSo4HivecfMS0_FT10otherQueenCSo1B_S0_ : $@cc(method) @thin (@owned B, @owned Hive) -> @owned Hive
  convenience init(otherQueen other: B) {
    // CHECK: [[META:%[0-9]+]] = value_metatype $@thick Hive.Type, [[SELF:%[0-9]+]] : $Hive
    // CHECK: [[FACTORY:%[0-9]+]] = class_method [volatile] [[META]] : $@thick Hive.Type, #Hive.init!allocator.1.foreign : Hive.Type -> (queen: B!) -> Hive! , $@cc(objc_method) @thin (ImplicitlyUnwrappedOptional<B>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
    // CHECK: [[OBJC_META:%[0-9]+]] = thick_to_objc_metatype [[META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
    // CHECK: apply [[FACTORY]]([[QUEEN:%[0-9]+]], [[OBJC_META]]) : $@cc(objc_method) @thin (ImplicitlyUnwrappedOptional<B>, @objc_metatype Hive.Type) -> @autoreleased ImplicitlyUnwrappedOptional<Hive>
    // CHECK-NOT: return %
    // CHECK: strong_release [[SELF]] : $Hive
    self.init(queen: other)
  }
}
