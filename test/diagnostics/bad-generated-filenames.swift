// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/SlashA.swiftmodule %S/Inputs/slash.swift
// RUN: %target-swift-frontend -emit-module -o %t/SlashB.swiftmodule %S/Inputs/slash.swift
// RUN: not %target-swift-frontend -typecheck -I %t -serialize-diagnostics-path %t/serialized.dia %s
// RUN: c-index-test -read-diagnostics %t/serialized.dia > %t/serialized.txt 2>&1
// RUN: %FileCheck %s -check-prefix CHECK-DIA < %t/serialized.txt

import SlashA
import SlashB

func test(a: String, b: Int) -> Bool {
  // CHECK-DIA: [[@LINE+3]]:5: error: ambiguous use of operator '/'
  // CHECK-DIA: SlashA./_operator
  // CHECK-DIA: SlashB./_operator
  a / b
}
