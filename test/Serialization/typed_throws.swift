// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name def_typed_throws -o %t %S/Inputs/def_typed_throws.swift
// RUN: llvm-bcanalyzer %t/def_typed_throws.swiftmodule | %FileCheck %s
// RUN: %target-typecheck-verify-swift -I %t

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -module-to-print=def_typed_throws -I %t -source-filename=%s | %FileCheck -check-prefix=CHECK-PRINT %s

// CHECK-NOT: UnknownCode

// CHECK-PRINT-DAG: func throwsMyError() throws(MyError)
// CHECK-PRINT-DAG: init() throws(MyError)
// CHECK-PRINT-DAG: var value: Int { get throws(MyError) }

import def_typed_throws

func testThrows() {
  let _: () -> Void = throwsMyError
  // expected-error@-1{{invalid conversion from throwing function of type '() throws(MyError) -> ()'}}

  let _: () -> Void = SomeStruct.init
  // expected-error@-1{{invalid conversion from throwing function of type '() throws(MyError) -> SomeStruct'}}
}
