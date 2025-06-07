// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/ -module-name Mixed -import-underlying-module %s -typecheck -verify -emit-objc-header-path %t/mixed.h
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=NO-IMPORT %s < %t/mixed.h
// RUN: %check-in-clang -F %S/Inputs/ %t/mixed.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name Mixed -import-objc-header %S/Inputs/Mixed.framework/Headers/Mixed.h %s -typecheck -verify -emit-objc-header-path %t/mixed-header.h
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=NO-IMPORT %s < %t/mixed-header.h
// RUN: %check-in-clang -include %S/Inputs/Mixed.framework/Headers/Mixed.h %t/mixed-header.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/ -module-name Mixed -import-underlying-module %s -typecheck -verify -emit-objc-header-path %t/mixed-proto.h -DREQUIRE
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=FRAMEWORK %s < %t/mixed-proto.h
// RUN: %check-in-clang -F %S/Inputs/ %t/mixed-proto.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name Mixed -import-objc-header %S/Inputs/Mixed.framework/Headers/Mixed.h %s -typecheck -verify -emit-objc-header-path %t/mixed-header-proto.h -DREQUIRE
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=HEADER %s < %t/mixed-header-proto.h
// RUN: %check-in-clang -include %S/Inputs/Mixed.framework/Headers/Mixed.h %t/mixed-header-proto.h

// REQUIRES: objc_interop

// CHECK-LABEL: #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning
// CHECK-NEXT: #pragma clang diagnostic
// CHECK-NEXT: #endif
// CHECK-NEXT: @import Foundation;
// CHECK-NEXT: #endif

// NO-IMPORT-NOT: #import
// NO-IMPORT: @protocol CustomProto;

// FRAMEWORK-LABEL: #import <Mixed/Mixed.h>
// HEADER-NOT: __ObjC
// HEADER: #import "{{.*}}Mixed.h"
// HEADER-NOT: __ObjC

import Foundation

public class Dummy: NSNumber {
  @objc public func getProto() -> CustomProto? {
    return nil
  }
}

#if REQUIRE
extension Dummy: CustomProto {}
#endif
