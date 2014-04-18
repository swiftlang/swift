// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -target x86_64-apple-darwin13 -module-cache-path %t/clang-module-cache -enable-objc-factory-method-constructors %s -emit-silgen | FileCheck %s

import Foundation

// CHECK-LABEL: sil shared @_TFCSo4HiveCfMS_FT9withQueenGSQCSo1B__S_ : $@thin (@owned UncheckedOptional<B>, @thick Hive.Type) -> @owned Hive
func testInstanceTypeFactoryMethod(queen: B) {
  // CHECK-NEXT: bb0([[QUEEN:%[0-9]+]] : $UncheckedOptional<B>, [[HIVE_META:%[0-9]+]] : $@thick Hive.Type):
  // CHECK-NEXT:   [[HIVE_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[HIVE_META]] : $@thick Hive.Type to $@objc_metatype Hive.Type
  // CHECK-NEXT:   [[FACTORY:%[0-9]+]] = class_method [[HIVE_META_OBJC]] : $@objc_metatype Hive.Type, #Hive.init!allocator.1.foreign : Hive.Type -> (withQueen: @unchecked B?) -> Hive , $@cc(objc_method) @thin (UncheckedOptional<B>, @objc_metatype Hive.Type) -> @autoreleased Hive
  // CHECK-NEXT:   [[HIVE:%[0-9]+]] = apply [[FACTORY]]([[QUEEN]], [[HIVE_META_OBJC]]) : $@cc(objc_method) @thin (UncheckedOptional<B>, @objc_metatype Hive.Type) -> @autoreleased Hive
// CHECK-NEXT:   strong_retain_autoreleased [[HIVE]] : $Hive
// CHECK-NEXT:   return [[HIVE]] : $Hive
  var hive1 = Hive(withQueen: queen)
}
