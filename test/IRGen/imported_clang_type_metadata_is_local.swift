// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend  -c -num-threads 1 -emit-ir -wmo -O  %t/useA.swift %t/useB.swift -I %t/Inputs -o %t/useA.swift.ir -o %t/useB.swift.ir
// RUN: cat %t/useB.swift.ir | %FileCheck %s

//--- Inputs/module.modulemap
module ClangModule {
    header "header.h"
}

//--- Inputs/header.h

struct ImportedClangType {
  int x;
};

//--- useA.swift

import ClangModule

func testA<T>(_ x: T) {
  print(x)
}

public func testARun() {
  testA(ImportedClangType(x: 0))
}

public struct TestUseFieldA {
  let x: ImportedClangType = ImportedClangType()
}

//--- useB.swift

import ClangModule

func testB<T>(_ x: T) {
  print(x)
}

public func testBRun() {
  testB(ImportedClangType(x: 0))
}

public struct TestUseFieldB {
  let x: ImportedClangType = ImportedClangType()
}

// The type metadata emitted for TU B, refers to the metadata from TU A,
// but it doesn't need to dllimport it across the module boundary, as it's
// emitted in the same module.
// CHECK: @"$sSo17ImportedClangTypeVMn" = external global %swift.type_descriptor
