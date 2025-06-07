// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden [ossa] @$s19ivar_destroyer_objc7ObjCFooCfETo : $@convention(objc_method) (ObjCFoo) -> () {
// CHECK: bb0(%0 : @unowned $ObjCFoo):
// CHECK:   [[CONVERT:%.*]] = unchecked_ownership_conversion %0 : $ObjCFoo, @unowned to @guaranteed
// CHECK:   ref_element_addr [[CONVERT]] : $ObjCFoo, #ObjCFoo.object
// CHECK:   begin_access [deinit] [static]
// CHECK:   destroy_addr
// CHECK:   end_access
// CHECK:   end_borrow [[CONVERT]] : $ObjCFoo
// CHECK-LABEL: } // end sil function '$s19ivar_destroyer_objc7ObjCFooCfETo'
internal class ObjCFoo : NSObject {
  var object: AnyObject
  init(o: AnyObject) { object = o }
}
