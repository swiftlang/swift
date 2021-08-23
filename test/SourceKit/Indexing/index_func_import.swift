// RUN: %empty-directory(%t)
// RUN: %swift -emit-module -o %t/test_module.swiftmodule %S/Inputs/test_module.swift -disable-implicit-concurrency-module-import

// RUN: %sourcekitd-test -req=index %s -- %s -I %t -Xfrontend -disable-implicit-concurrency-module-import | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

import func test_module.globalFunc

func test() {
  globalFunc()
}
