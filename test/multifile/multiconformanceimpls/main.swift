// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -emit-module %S/Inputs/A.swift -o %t/libA.%target-dylib-extension -emit-module-path %t/A.swiftmodule -module-name A
// RUN: %target-codesign %t/libA.%target-dylib-extension
// RUN: %target-build-swift -emit-library -emit-module %S/Inputs/B.swift -o %t/libB.%target-dylib-extension -emit-module-path %t/B.swiftmodule -module-name B -I %t %t/libA.%target-dylib-extension
// RUN: %target-codesign %t/libB.%target-dylib-extension
// RUN: %target-build-swift -emit-library -emit-module %S/Inputs/C.swift -o %t/libC.%target-dylib-extension -emit-module-path %t/C.swiftmodule -module-name C -I %t %t/libA.%target-dylib-extension
// RUN: %target-codesign %t/libC.%target-dylib-extension
// RUN: %target-build-swift %s -I %t %t/libA.%target-dylib-extension %t/libB.%target-dylib-extension %t/libC.%target-dylib-extension -o %t/a.out
// RUN: %target-run %t/a.out |  %FileCheck %s

import A
import B
import C

func runTheTest() {
  setAnyWithImplFromB()
// CHECK: check returned not Container<Impl> in C
  checkAnyInC()
}

runTheTest()
