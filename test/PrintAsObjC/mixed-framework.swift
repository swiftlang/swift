// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -F %S/Inputs/ -module-cache-path %t/clang-module-cache -module-name Mixed -import-underlying-module %s -parse -emit-objc-header-path %t/mixed.h
// RUN: FileCheck %s < %t/mixed.h
// RUN: %check-in-clang -F %S/Inputs/ %t/mixed.h

// CHECK-NOT: @import Swift;

// CHECK-LABEL: #if defined(__has_feature) && __has_feature(modules)
// CHECK-NEXT: @import Foundation;
// CHECK-NEXT: #endif

// CHECK-LABEL: #import <Mixed/Mixed.h>

import Foundation

class Dummy: NSNumber {
  func getIntAlias() -> CIntAlias {
    let result: CInt = 0
    return result
  }
}
