// RUN: %empty-directory(%t)
// RUN: cp -R %S/Inputs/VerySmallObjCFramework.framework %t
// RUN: %clang -dynamiclib %S/Inputs/VerySmallObjCFramework.m -fmodules -F %t -o %t/VerySmallObjCFramework.framework/VerySmallObjCFramework -L %sdk/usr/lib
// RUN: %swift_driver -F %t %s | %FileCheck %s

// REQUIRES: objc_interop 
// REQUIRES: swift_interpreter

import VerySmallObjCFramework

// CHECK: 42
print(globalValue)
