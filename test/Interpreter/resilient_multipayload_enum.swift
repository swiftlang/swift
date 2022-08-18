// Build unoptimized library
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ResilientEnum)) -enable-library-evolution %S/Inputs/resilient_multipayload_enum.swift -emit-module -emit-module-path %t/ResilientEnum.swiftmodule -module-name ResilientEnum
// RUN: %target-codesign %t/%target-library-name(ResilientEnum)
// RUN: %target-build-swift %s -lResilientEnum -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(ResilientEnum) | %FileCheck %s

// Build optimized library (this exercises a different value witness generation path)
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ResilientEnum)) -enable-library-evolution %S/Inputs/resilient_multipayload_enum.swift -emit-module -emit-module-path %t/ResilientEnum.swiftmodule -module-name ResilientEnum -O
// RUN: %target-codesign %t/%target-library-name(ResilientEnum)
// RUN: %target-build-swift %s -lResilientEnum -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(ResilientEnum) | %FileCheck %s

// REQUIRES: executable_test

import ResilientEnum

@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func dump<T>(_ t: T) {
    print(t)
}

@inline(never)
func doit() {
  let e = ProblematicEnumeration<Bool>.six
  // CHECK: six
  dump(e)
}

doit()
