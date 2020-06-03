// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-silgen -module-name SomeModule %S/Inputs/objc_dynamic_replacement_ext.swift -swift-version 5 -enable-implicit-dynamic | %FileCheck %s --check-prefix=IMPORT
// RUN: %target-swift-emit-silgen -module-name SomeModule %S/Inputs/objc_dynamic_replacement_ext.swift -swift-version 5 | %FileCheck %s --check-prefix=NO
// RUN: %target-swift-frontend -module-name SomeModule -emit-module -emit-module-path=%t/SomeModule.swiftmodule %S/Inputs/objc_dynamic_replacement_ext.swift -swift-version 5 -validate-tbd-against-ir=all
// RUN: %target-swift-frontend -module-name SomeModule -emit-module -emit-module-path=%t/SomeModule.swiftmodule %S/Inputs/objc_dynamic_replacement_ext.swift -swift-version 5 -enable-implicit-dynamic -validate-tbd-against-ir=all
// RUN: %target-swift-emit-silgen -I %t %s -swift-version 5 | %FileCheck %s
// RUN: %target-swift-emit-ir -I %t %s -swift-version 5 -validate-tbd-against-ir=all

// REQUIRES: objc_interop

import Foundation
import SomeModule

// Make sure we support replacing @objc dynamic methods in generic classes.
// Normally we would disallow such methods in extensions because we don't
// support emitting objc categories for native generic classes. We special case
// @_dynamicReplacements for such methods and use the native dynamic replacement
// mechanism instead.

// In imported file:
// public class Generic<ItemType>: NSObject {
//   @objc public dynamic func foo() {}
//   @objc public dynamic var x: Int {
//     get {
//       return 0;
//     }
//     set {
//       print("noop")
//     }
//   }
//   @objc public dynamic var y: Int = 0
// }

// IMPORT-DAG: sil [dynamically_replacable] [ossa] @$s10SomeModule7GenericC3fooyyF : $@convention(method) <ItemType> (@guaranteed Generic<ItemType>) -> ()
// IMPORT-DAG: sil [thunk] [ossa] @$s10SomeModule7GenericC3fooyyFTo : $@convention(objc_method) <ItemType> (Generic<ItemType>) -> ()

// IMPORT-DAG: sil [thunk] [ossa] @$s10SomeModule7GenericC1xSivgTo : $@convention(objc_method) <ItemType> (Generic<ItemType>) -> Int
// IMPORT-DAG: sil [dynamically_replacable] [ossa] @$s10SomeModule7GenericC1xSivg : $@convention(method) <ItemType> (@guaranteed Generic<ItemType>) -> Int
// IMPORT-DAG: sil [thunk] [ossa] @$s10SomeModule7GenericC1xSivsTo : $@convention(objc_method) <ItemType> (Int, Generic<ItemType>) -> ()
// IMPORT-DAG: sil [dynamically_replacable] [ossa] @$s10SomeModule7GenericC1xSivs : $@convention(method) <ItemType> (Int, @guaranteed Generic<ItemType>) -> ()

// NO-DAG: sil [thunk] [ossa] @$s10SomeModule7GenericC1xSivgTo : $@convention(objc_method) <ItemType> (Generic<ItemType>) -> Int
// NO-DAG: sil [ossa] @$s10SomeModule7GenericC1xSivg : $@convention(method) <ItemType> (@guaranteed Generic<ItemType>) -> Int
// NO-DAG: sil [thunk] [ossa] @$s10SomeModule7GenericC1xSivsTo : $@convention(objc_method) <ItemType> (Int, Generic<ItemType>) -> ()
// NO-DAG: sil [ossa] @$s10SomeModule7GenericC1xSivs : $@convention(method) <ItemType> (Int, @guaranteed Generic<ItemType>) -> ()

// IMPORT-DAG: sil [thunk] [ossa] @$s10SomeModule7GenericC1ySivgTo : $@convention(objc_method) <ItemType> (Generic<ItemType>) -> Int
// IMPORT-DAG: sil [dynamically_replacable] [ossa] @$s10SomeModule7GenericC1ySivg : $@convention(method) <ItemType> (@guaranteed Generic<ItemType>) -> Int
// IMPORT-DAG: sil [thunk] [ossa] @$s10SomeModule7GenericC1ySivsTo : $@convention(objc_method) <ItemType> (Int, Generic<ItemType>) -> ()
// IMPORT-DAG: sil [dynamically_replacable] [ossa] @$s10SomeModule7GenericC1ySivs : $@convention(method) <ItemType> (Int, @guaranteed Generic<ItemType>) -> ()

extension Generic {
  @_dynamicReplacement(for: foo()) public func __replacement__foo() {}
// CHECK-DAG: sil [dynamic_replacement_for "$s10SomeModule7GenericC3fooyyF"] [ossa] @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F5__fooyyF : $@convention(method) <ItemType> (@guaranteed Generic<ItemType>) -> ()
// CHECK-NOT: sil {{.*}} @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F5__fooyyFTo : $@convention(objc_method) <ItemType> (Generic<ItemType>) -> ()

  @_dynamicReplacement(for: x) public var __replacement_x : Int {
    get {
      return 0;
    }
    set {
      print("noop")
    }
  }
// CHECK-NOT: sil {{.*}} @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_xSivgTo : $@convention(objc_method) <ItemType> (Generic<ItemType>) -> Int
// CHECK-DAG: sil [dynamic_replacement_for "$s10SomeModule7GenericC1xSivg"] [ossa] @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_xSivg : $@convention(method) <ItemType> (@guaranteed Generic<ItemType>) -> Int
// CHECK-NOT: sil {{.*}} @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_xSivsTo : $@convention(objc_method) <ItemType> (Int, Generic<ItemType>) -> ()
// CHECK-DAG: sil [dynamic_replacement_for "$s10SomeModule7GenericC1xSivs"] [ossa] @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_xSivs : $@convention(method) <ItemType> (Int, @guaranteed Generic<ItemType>) -> ()

  @_dynamicReplacement(for: y) public var __replacement_y : Int {
    get {
      return 7;
    }
    set {
    }
  }
// CHECK-NOT: sil {{.*}} @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_ySivgTo : $@convention(objc_method) <ItemType> (Generic<ItemType>) -> Int
// CHECK-DAG: sil [dynamic_replacement_for "$s10SomeModule7GenericC1ySivg"] [ossa] @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_ySivg : $@convention(method) <ItemType> (@guaranteed Generic<ItemType>) -> Int
// CHECK-NOT: sil {{.*}} @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_ySivsTo : $@convention(objc_method) <ItemType> (Int, Generic<ItemType>) -> ()
// CHECK-DAG: sil [dynamic_replacement_for "$s10SomeModule7GenericC1ySivs"] [ossa] @$s10SomeModule7GenericC28objc_dynamic_replacement_extE02__F2_ySivs : $@convention(method) <ItemType> (Int, @guaranteed Generic<ItemType>) -> ()
}
