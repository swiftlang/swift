// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

// rdar://21184674

import ObjCClasses

// FIXME: this is necessary to get the bridging code, which is dumb.
import Foundation

do {
  try NilError.throwIt()
} catch {
  // CHECK: error was: Foundation._GenericObjCError.NilError
  print("error was: \(error)")
}
