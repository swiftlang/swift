// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -F %S/Inputs/ -module-cache-path %t/clang-module-cache -module-name Mixed -import-underlying-module %s -parse -emit-objc-header-path %t/mixed.h
// RUN: FileCheck -check-prefix=CHECK -check-prefix=FRAMEWORK %s < %t/mixed.h
// RUN: %check-in-clang -F %S/Inputs/ %t/mixed.h

// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -module-name Mixed -import-objc-header %S/Inputs/Mixed.framework/Headers/Mixed.h %s -parse -emit-objc-header-path %t/mixed-header.h
// RUN: FileCheck -check-prefix=CHECK -check-prefix=HEADER %s < %t/mixed-header.h
// RUN: %check-in-clang -include %S/Inputs/Mixed.framework/Headers/Mixed.h %t/mixed-header.h

// CHECK-LABEL: #if defined(__has_feature) && __has_feature(modules)
// CHECK-NEXT: @import Foundation;
// CHECK-NEXT: #endif

// FRAMEWORK-LABEL: #import <Mixed/Mixed.h>
// HEADER-NOT: __ObjC

import Foundation

class Dummy: NSNumber {
  func getIntAlias() -> CIntAlias {
    let result: CInt = 0
    return result
  }
}
