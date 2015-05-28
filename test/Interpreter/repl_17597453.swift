// RUN: rm -rf %t && mkdir -p %t
// RUN: cp -R %S/Inputs/VerySmallObjCFramework.framework %t
// RUN: %clang -dynamiclib %S/Inputs/VerySmallObjCFramework.m -fmodules -F %t -o %t/VerySmallObjCFramework.framework/VerySmallObjCFramework
// RUN: %target-repl-run-simple-swift -F %t | FileCheck %s

// REQUIRES: swift_repl
// REQUIRES: objc_interop 

import simd
import VerySmallObjCFramework

// CHECK: 1171
print(staticGlobalValue)

// CHECK: float4(1.0, 2.0, 3.0, 4.0)
print(staticGlobalValueSIMD)
