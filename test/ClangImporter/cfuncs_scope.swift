// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules 2>&1 | %FileCheck %s

import LocalVsFileScope

func testLocalVsFileScope() {
  LocalVsFileScopeBase.theFunctionInQuestion()
  // CHECK: :[[@LINE-1]]:3: error: module 'LocalVsFileScopeBase' has no member named 'theFunctionInQuestion'

  theFunctionInQuestion()
  // CHECK: :[[@LINE-1]]:25: error: missing argument
  // CHECK: LocalVsFileScope.theFunctionInQuestion:1:{{[0-9]+}}: note:
  // This is not a wonderful test because it's relying on the diagnostic
  // engine's synthesis of fake declarations to figure out what module the
  // importer assigned the function to. But, well, that's what we get.

  aFunctionInBase() // just make sure it's imported
  // CHECK-NOT: :[[@LINE-1]]:{{[0-9]+}}:
}
