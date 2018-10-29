// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libA.%target-dylib-extension) %S/Inputs/A.swift -emit-module -emit-module-path %t/A.swiftmodule -module-name A
// RUN: %target-codesign %t/libA.%target-dylib-extension
// RUN: %target-build-swift-dylib(%t/libB.%target-dylib-extension) %S/Inputs/B.swift -emit-module -emit-module-path %t/B.swiftmodule -module-name B -I%t -L%t -lA
// RUN: %target-codesign %t/libB.%target-dylib-extension
// RUN: %target-build-swift-dylib(%t/libC.%target-dylib-extension) %S/Inputs/C.swift -emit-module -emit-module-path %t/C.swiftmodule -module-name C -I%t -L%t -lA
// RUN: %target-codesign %t/libC.%target-dylib-extension
// RUN: %target-build-swift %s -I %t -o %t/a.out -L %t -Xlinker -rpath -Xlinker %t -lA -lB -lC
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/libA.%target-dylib-extension %t/libB.%target-dylib-extension %t/libC.%target-dylib-extension |  %FileCheck %s

// REQUIRES: executable_test

import A
import B
import C

func runTheTest() {
  setAnyWithImplFromB()
// CHECK: check returned not Container<Impl> in C
  checkAnyInC()
}

runTheTest()
