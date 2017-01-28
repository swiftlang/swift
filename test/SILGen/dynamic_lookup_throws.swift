// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -Xllvm -new-mangling-for-tests -emit-silgen -parse-as-library %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class Blub : NSObject {
   func blub() throws {}
}

// CHECK-LABEL: sil hidden @_T021dynamic_lookup_throws8testBlubys9AnyObject_p1a_tKF : $@convention(thin) (@owned AnyObject) -> @error Error
func testBlub(a: AnyObject) throws {
  // CHECK:   open_existential_ref %0 : $AnyObject to $@opened("[[OPENED:.*]]") AnyObject
  // CHECK:   dynamic_method [volatile] %4 : $@opened("[[OPENED]]") AnyObject, #Blub.blub!1.foreign : (Blub) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @opened("[[OPENED]]") AnyObject) -> ObjCBool
  // CHECK:   cond_br {{%.*}}, bb1, bb2

  // CHECK: bb1
  // CHECK:   return

  // CHECK: bb2
  // CHECK:   function_ref @swift_convertNSErrorToError
  // CHECK:   throw {{%.*}} : $Error
  try a.blub()
}
