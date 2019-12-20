// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/../Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/../Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import ObjCClasses
import Foundation

TestingBool().shouldBeTrueObjCBool(true)
TestingBool().shouldBeTrueCBool(true)
