// RUN: %empty-directory(%t)
// RUN: %swift -emit-module -o %t/test_module.swiftmodule %S/Inputs/test_module.swift

// RUN: %sourcekitd-test -req=index %s -- %s -I %t | %sed_clean > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response

import func test_module.globalFunc

func test() {
  globalFunc()
}
