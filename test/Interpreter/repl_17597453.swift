// RUN: rm -rf %t && mkdir -p %t
// RUN: cp -R %S/Inputs/VerySmallObjCFramework.framework %t
// RUN: %clang -dynamiclib %S/Inputs/VerySmallObjCFramework.m -fmodules -F %t -o %t/VerySmallObjCFramework.framework/VerySmallObjCFramework
// RUN: %target-repl-run-simple-swift -F %t | FileCheck %s

// REQUIRES: swift_repl
// REQUIRES: objc_interop 

import VerySmallObjCFramework

// CHECK: 1171
print(staticGlobalValue)
