// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | FileCheck %s
// REQUIRES: objc_interop

import objc_generics

func callInitializer() {
  _ = GenericClass(thing: NSObject())
}

// CHECK-LABEL: sil shared @_TFCSo12GenericClassCfT5thingGSQx__GSQGS_x__
// CHECK:         thick_to_objc_metatype {{%.*}} : $@thick GenericClass<T>.Type to $@objc_metatype GenericClass<T>.Type
