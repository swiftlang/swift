// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/NativeDep.framework/Headers %t/NativeDep.framework/Modules %t/cache
// RUN: cp %S/Inputs/module.modulemap %t/NativeDep.framework/Modules
// RUN: cp %S/Inputs/NativeDep.h %t/NativeDep.framework/Headers

// RUN: %target-swift-frontend %s -emit-module -emit-module-interface-path %t/MyModule.swiftinterface -emit-module-path %t/MyModule.swiftmodule -F %t -enable-library-evolution -module-cache-path %t/cache -module-name MyModule -swift-version 5

/// Check that both swiftmodule and swiftinterface can be used as input.
// RUN: %target-swift-api-extract -o - -pretty-print %t/MyModule.swiftmodule -module-name MyModule -module-cache-path %t/cache -F %t | %FileCheck %s
// RUN: %target-swift-api-extract -o - -pretty-print %t/MyModule.swiftinterface -module-name MyModule -module-cache-path %t/cache -F %t | %FileCheck %s

/// Check that if a dependency is missing, error message is emitted and not crashed.
// RUN: rm -rf %t/NativeDep.framework
// RUN: not %target-swift-api-extract -o - -pretty-print %t/MyModule.swiftmodule -module-name MyModule -module-cache-path %t/cache 2>&1 | %FileCheck %s --check-prefix=CHECK-ERROR

import NativeDep

public func callNative ()
{
      my_native_c()      
}

// CHECK:        "target": 
// CHECK-NEXT:   "globals": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule10callNativeyyF",
// CHECK-NEXT:       "access": "public",

// CHECK-ERROR: error: missing required module 'NativeDep'
