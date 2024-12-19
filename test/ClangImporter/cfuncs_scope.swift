// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules 2>&1 | %FileCheck %s

import LocalVsFileScope

func testLocalVsFileScope() {
  LocalVsFileScopeBase.theFunctionInQuestion()
  // CHECK: :[[@LINE-1]]:3: error: module 'LocalVsFileScopeBase' has no member named 'theFunctionInQuestion'

  theFunctionInQuestion()
  // CHECK: :[[@LINE-1]]:25: error: missing argument
  // CHECK: LocalVsFileScope.h:{{[0-9]+}}:{{[0-9]+}}: note:

  aFunctionInBase() // just make sure it's imported
  // CHECK-NOT: :[[@LINE-1]]:{{[0-9]+}}:
}
