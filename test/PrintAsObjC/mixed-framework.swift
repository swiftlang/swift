// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -F %S/Inputs/ -module-name Mixed -import-underlying-module -parse-as-library %s -parse -emit-objc-header-path %t/mixed.h
// RUN: FileCheck -check-prefix=CHECK -check-prefix=FRAMEWORK %s < %t/mixed.h
// RUN: %check-in-clang -F %S/Inputs/ %t/mixed.h

// RUN: %target-swift-frontend %clang-importer-sdk -module-name Mixed -import-objc-header %S/Inputs/Mixed.framework/Headers/Mixed.h %s -parse -emit-objc-header-path %t/mixed-header.h
// RUN: FileCheck -check-prefix=CHECK -check-prefix=HEADER %s < %t/mixed-header.h
// RUN: %check-in-clang -include %S/Inputs/Mixed.framework/Headers/Mixed.h %t/mixed-header.h

// REQUIRES: objc_interop

// CHECK-LABEL: #if defined(__has_feature) && __has_feature(modules)
// CHECK-NEXT: @import Foundation;
// CHECK-NEXT: #endif

// FRAMEWORK-LABEL: #import <Mixed/Mixed.h>
// HEADER-NOT: __ObjC
// HEADER: #import "{{.*}}Mixed.h"
// HEADER-NOT: __ObjC

import Foundation

// CHECK-LABEL: @interface Dummy : NSNumber
public class Dummy: NSNumber {
  // CHECK: - (CIntAlias)getIntAlias;
  public func getIntAlias() -> CIntAlias {
    let result: CInt = 0
    return result
  }

  // FRAMEWORK: @property (nonatomic, readonly) NSInteger extraData;
  // HEADER: @property (nonatomic) NSInteger extraData;
  public internal(set) var extraData: Int = 0
} // CHECK: @end
