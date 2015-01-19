// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/multi-file-2.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/multi-file-2.swift

// RUN: llvm-bcanalyzer %t/multi-file.swiftmodule | FileCheck %s -check-prefix=THIS-FILE
// RUN: llvm-bcanalyzer %t/multi-file.swiftmodule | FileCheck %s -check-prefix=THIS-FILE-NEG
// RUN: llvm-bcanalyzer %t/multi-file-2.swiftmodule | FileCheck %s -check-prefix=OTHER-FILE
// RUN: llvm-bcanalyzer %t/multi-file-2.swiftmodule | FileCheck %s -check-prefix=OTHER-FILE-NEG

// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule -o %t
// RUN: llvm-bcanalyzer %t/Multi.swiftmodule | FileCheck %s -check-prefix=THIS-FILE
// RUN: llvm-bcanalyzer %t/Multi.swiftmodule | FileCheck %s -check-prefix=OTHER-FILE

// Do not put any enums in this file. It's part of the test that no enums
// get serialized here.

class MyClass {
  var value: TheEnum = .A
}

func foo<T: Equatable>(x: T) {}
func bar() {
  foo(EquatableEnum.A)
}

// THIS-FILE: CLASS_DECL
// OTHER-FILE-NEG-NOT: CLASS_DECL
// OTHER-FILE: ENUM_DECL
// THIS-FILE-NEG-NOT: ENUM_DECL


// <rdar://problem/17251682>
struct StructWithInheritedConformances: SequenceType {
  struct EmptyGenerator: GeneratorType {
    mutating func next() -> Int? {
      return nil
    }
  }

  func generate() -> EmptyGenerator {
    return EmptyGenerator()
  }
}
