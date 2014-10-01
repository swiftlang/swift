// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation
class MyFunkyDictionary: NSDictionary {
  // CHECK-LABEL: sil hidden @_TFC18super_class_method17MyFunkyDictionary10initializefMS0_FT_T_
  // CHECK:         super_method [volatile] %0 : $@thick MyFunkyDictionary.Type, #NSObject.initialize!1.foreign : NSObject.Type -> () -> ()
  override class func initialize() {
    super.initialize()
  }
}

