
// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk-nosource -I %t) -module-name dynamic_lookup_throws -parse-as-library %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class Blub : NSObject {
   @objc func blub() throws {}
}

// CHECK-LABEL: sil hidden @$s21dynamic_lookup_throws8testBlub1ayyXl_tKF : $@convention(thin) (@guaranteed AnyObject) -> @error Error
// CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
func testBlub(a: AnyObject) throws {
  // CHECK:   [[ANYOBJECT_REF:%.*]] = open_existential_ref [[ARG]] : $AnyObject to $@opened("[[OPENED:.*]]") AnyObject
  // CHECK:   [[ANYOBJECT_REF_COPY:%.*]] = copy_value [[ANYOBJECT_REF]]
  // CHECK:   objc_method [[ANYOBJECT_REF_COPY]] : $@opened("[[OPENED]]") AnyObject, #Blub.blub!1.foreign : (Blub) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @opened("[[OPENED]]") AnyObject) -> ObjCBool
  // CHECK:   cond_br {{%.*}}, bb1, bb2

  // CHECK: bb1
  // CHECK:   return

  // CHECK: bb2
  // CHECK:   function_ref @$s10Foundation22_convertNSErrorToErrorys0E0_pSo0C0CSgF
  // CHECK:   throw {{%.*}} : $Error
  try a.blub()
}
