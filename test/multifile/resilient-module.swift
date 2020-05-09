// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(A)) -enable-library-evolution -module-name A -emit-module -emit-module-path %t/A.swiftmodule %S/Inputs/resilient-module-2.swift
// RUN: %target-swift-frontend -enable-library-evolution -module-name A %S/Inputs/resilient-module-2.swift -emit-ir | %FileCheck --check-prefix=METADATA %s
// RUN: %target-build-swift -I%t -L%t -lA -o %t/main %target-rpath(%t) %s
// RUN: %target-build-swift -I%t -L%t -lA -o %t/main %target-rpath(%t) %s
// RUN: %target-codesign %t/main %t/%target-library-name(A)
// RUN: %target-run %t/main %t/%target-library-name(A) | %FileCheck %s

// REQUIRES: executable_test

// METADATA: @"$s1A8SomeEnumOMn" = {{.*}}constant <{ i32, i32, i32, i32, i32, i32, i32 }> <{{{.*}} i32 33554434, i32 0 }>

import A

func runTest() {
  let e = SomeEnum.first(ResilientType.a(Int64(10)))
  // CHECK: first(A.ResilientType.a(10))
  print(e)
}

runTest()
