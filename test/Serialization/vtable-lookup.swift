// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library -module-name Classes -emit-module -emit-module-path %t/Classes.swiftmodule %S/Inputs/classes.swift
// RUN: %target-build-swift -parse-as-library -module-name Classes -c -o %t/classes.o %S/Inputs/classes.swift
// RUN: %target-build-swift -I %t %s %t/classes.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

import Classes

// Check if the right vtable is deserialized in case of two classes which have
// the same name (but are in different contexts).

func testit() {
  let a = StructA.Testclass()
  // CHECK: true
  print(type(of: a) == StructA.Testclass.self)
}

extension StructA.Testclass {
    convenience init() {
        self.init(data: 0)
    }
}

testit()

