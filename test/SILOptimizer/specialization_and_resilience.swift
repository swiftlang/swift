// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -wmo -O -enable-library-evolution %S/Inputs/specialization_and_resilience_module.swift -DMODULE -parse-as-library -emit-module -emit-module-path=%t/Test.swiftmodule -module-name=Test -c -o %t/module.o
// RUN: %target-build-swift -wmo -O %s -I%t -module-name=Main -c -o %t/main.o
// RUN: %target-build-swift %t/main.o %t/module.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

import Test

// CHECK: Mystruct(x: 100)
testParam(Mystruct(100))
// CHECK: Mystruct(x: 101)
print(testReturn([Mystruct(101)]))
// CHECK: Mystruct(x: 27)
// CHECK: Mystruct(x: 28)
otherFunc()

