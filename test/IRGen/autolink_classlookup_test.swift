// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/../Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: mkdir -p %t/ObjCClasses.framework/Headers
// RUN: cp %S/../Inputs/ObjCClasses/ObjCClasses.h %t/ObjCClasses.framework/Headers
// RUN: mkdir -p %t/ObjCClasses.framework/Modules
// RUN: cp %S/../Inputs/ObjCClasses/framework.module.map %t/ObjCClasses.framework/Modules/module.modulemap
// RUN: %target-clang -dynamiclib -fobjc-arc -fmodules %t/ObjCClasses.o -o %t/ObjCClasses.framework/ObjCClasses
// RUN: %target-codesign %t/ObjCClasses.framework/ObjCClasses

// RUN: %target-build-swift -emit-ir -F %t %s -o - | %FileCheck %s

// REQUIRES: objc_interop

// Make sure that we have our class ref here.
// CHECK-DAG: @"OBJC_CLASS_REF_$_Animal"

// And make sure we have ObjCClasses in our autolinking info.
//
// CHECK-DAG: !{{[0-9][0-9]*}} = !{!"-framework", !"ObjCClasses"}

// Make sure that we emit an objc class ref to Animal so autolinking does not
// fail.

import ObjCClasses

func main() {
  let a = Animal()
  print(a)
}

main()
