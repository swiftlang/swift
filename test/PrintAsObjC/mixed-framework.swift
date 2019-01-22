// RUN: %empty-directory(%t.framework)
// RUN: %empty-directory(%t.framework/Headers)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/ -module-name Mixed -import-underlying-module -parse-as-library %s -typecheck -emit-objc-header-path %t.framework/Headers/mixed.h
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=FRAMEWORK %s < %t.framework/Headers/mixed.h
// RUN: %check-in-clang -Watimport-in-framework-header -F %S/Inputs/ %t.framework/Headers/mixed.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name Mixed -import-objc-header %S/Inputs/Mixed.framework/Headers/Mixed.h %s -typecheck -emit-objc-header-path %t.framework/Headers/mixed-header.h
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=HEADER %s < %t.framework/Headers/mixed-header.h
// RUN: %check-in-clang -Watimport-in-framework-header -include %S/Inputs/Mixed.framework/Headers/Mixed.h %t.framework/Headers/mixed-header.h

// REQUIRES: objc_interop

// CHECK: #pragma clang diagnostic push

// CHECK-LABEL: #if __has_feature(modules)
// CHECK-NEXT: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT: #endif
// CHECK-NEXT: @import Foundation;
// CHECK-NEXT: #endif

// FRAMEWORK-LABEL: #import <Mixed/Mixed.h>
// HEADER-NOT: __ObjC
// HEADER: #import "{{.*}}Mixed.h"
// HEADER-NOT: __ObjC

import Foundation

// CHECK-LABEL: @interface Dummy : NSNumber
public class Dummy: NSNumber {
  // CHECK: - (CIntAlias)getIntAlias SWIFT_WARN_UNUSED_RESULT;
  @objc public func getIntAlias() -> CIntAlias {
    let result: CInt = 0
    return result
  }

  // FRAMEWORK: @property (nonatomic, readonly) NSInteger extraData;
  // HEADER: @property (nonatomic) NSInteger extraData;
  @objc public internal(set) var extraData: Int = 0
} // CHECK: @end

// CHECK: #pragma clang diagnostic pop
