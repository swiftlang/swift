// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift-dylib(%t/%target-library-name(A)) %S/Inputs/A.swift -emit-module -emit-module-path %t/A.swiftmodule -module-name A
// RUN: %target-codesign %t/%target-library-name(A)
//
// RUN: %target-build-swift-dylib(%t/%target-library-name(B)) %S/Inputs/B.swift -emit-module -emit-module-path %t/B.swiftmodule -module-name B -I%t -L%t -lA
// RUN: %target-codesign %t/%target-library-name(B)
//
// RUN: %target-build-swift-dylib(%t/%target-library-name(C)) %S/Inputs/C.swift -emit-module -emit-module-path %t/C.swiftmodule -module-name C -I%t -L%t -lA
// RUN: %target-codesign %t/%target-library-name(C)
//
// RUN: %target-build-swift %s -I %t -o %t/main.out -L %t %target-rpath(%t) -lA -lB -lC
// RUN: %target-codesign %t/main.out
//
// RUN: %target-run %t/main.out %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(C) |  %FileCheck %s

// REQUIRES: executable_test

// FIXME: Fetching metadata by mangled name does not consider the provenance of
// retroactive conformances. rdar://problem/53828345
// REQUIRES: 53828345

import A
import B
import C

func runTheTest() {
  setAnyWithImplFromB()
// CHECK: check returned not Container<Impl> in C
  checkAnyInC()
}

runTheTest()
