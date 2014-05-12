// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/multi-file-2.swift
// RUN: %swift -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/multi-file-2.swift

// RUN: llvm-bcanalyzer %t/multi-file.swiftmodule | FileCheck %s -check-prefix=THIS-FILE
// RUN: llvm-bcanalyzer %t/multi-file.swiftmodule | FileCheck %s -check-prefix=THIS-FILE-NEG
// RUN: llvm-bcanalyzer %t/multi-file-2.swiftmodule | FileCheck %s -check-prefix=OTHER-FILE
// RUN: llvm-bcanalyzer %t/multi-file-2.swiftmodule | FileCheck %s -check-prefix=OTHER-FILE-NEG

// RUN: %swift -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule -o %t
// RUN: llvm-bcanalyzer %t/Multi.swiftmodule | FileCheck %s -check-prefix=THIS-FILE
// RUN: llvm-bcanalyzer %t/Multi.swiftmodule | FileCheck %s -check-prefix=OTHER-FILE

// Do not put any enums in this file. It's part of the test that no enums
// get serialized here.

class MyClass {
  var value: TheEnum = .A
}

let equatableGlobal: Equatable = EquatableEnum.A

// THIS-FILE: CLASS_DECL
// OTHER-FILE-NEG-NOT: CLASS_DECL
// OTHER-FILE: ENUM_DECL
// THIS-FILE-NEG-NOT: ENUM_DECL
