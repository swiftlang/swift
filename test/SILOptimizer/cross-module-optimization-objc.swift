// First test: functional correctness

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -wmo -parse-as-library -cross-module-optimization -emit-module -emit-module-path=%t/Test.swiftmodule -module-name=Test -I%t %S/Inputs/cross-module-objc.swift -c -o %t/test.o
// RUN: %target-build-swift -O -wmo -module-name=Main -I%t %s -c -o %t/main.o
// RUN: %target-swiftc_driver %t/main.o %t/test.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// Check if it also works if the main module is compiled with -Onone:

// RUN: %target-build-swift -Onone -wmo -module-name=Main -I%t %s -c -o %t/main-onone.o
// RUN: %target-swiftc_driver %t/main-onone.o %t/test.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test
// REQUIRES: objc_interop

// Second test: check if CMO really imports the SIL of functions in other modules.

// RUN: %target-build-swift -O -wmo -module-name=Main -I%t %s -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil | %FileCheck %s -check-prefix=CHECK-SIL


import Test

func testClass() {
  // CHECK-OUTPUT: 127
  // CHECK-SIL-DAG: sil shared [noinline] @$s4Test21returnObjcClassMemberySiAA0cD0C_xtlFSi_Tg5
  print(callObjcClassMember(0))
}

testClass()

