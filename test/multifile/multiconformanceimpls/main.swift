// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}A%{target-shared-library-suffix}) %S/Inputs/A.swift -emit-module -emit-module-path %t/A.swiftmodule -module-name A
// RUN: %target-codesign %t/%target-library-name(A)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}B%{target-shared-library-suffix}) %S/Inputs/B.swift -emit-module -emit-module-path %t/B.swiftmodule -module-name B -I%t -L%t -lA
// RUN: %target-codesign %t/%target-library-name(B)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}C%{target-shared-library-suffix}) %S/Inputs/C.swift -emit-module -emit-module-path %t/C.swiftmodule -module-name C -I%t -L%t -lA
// RUN: %target-codesign %t/%target-library-name(C)
// RUN: %target-build-swift %s -I %t -o %t/a.out -L %t -Xlinker -rpath -Xlinker %t -lA -lB -lC
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(C) |  %FileCheck %s

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
