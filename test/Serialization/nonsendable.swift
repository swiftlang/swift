// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name def_nonsendable -o %t %S/Inputs/def_nonsendable.swift
// RUN: llvm-bcanalyzer %t/def_nonsendable.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -I %t %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -I %t %s -o /dev/null

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -module-to-print=def_nonsendable -I %t -source-filename=%s | %FileCheck -check-prefix=CHECK-PRINT %s

// CHECK-NOT: UnknownCode

// CHECK-PRINT-DAG: @_nonSendable struct A
// CHECK-PRINT-DAG: @_nonSendable(_assumed) struct B

import def_nonsendable

func test(a: A, b: B) { }
