// REQUIRES: OS=macosx || OS=linux-gnu

func foo() -> String {
  // RUN: %sourcekitd-test -req=localize-string -pos=%(line+1):10 %s -- %s | %FileCheck %s --check-prefix=CHECK-BASIC
  return "abc"
  // CHECK-BASIC: source.edit.kind.active:
  // CHECK-BASIC: [[# @LINE-2]]:10-[[# @LINE-2]]:10 "NSLocalizedString("
  // CHECK-BASIC: source.edit.kind.active:
  // CHECK-BASIC: [[# @LINE-4]]:15-[[# @LINE-4]]:15 ", comment: "")"
}

#sourceLocation(file: "someFile.swift", line: 20)
func bar() -> String {
  // RUN: %sourcekitd-test -req=localize-string -pos=%(line+1):10 %s -- %s | %FileCheck %s --check-prefix=CHECK-DIRECTIVE
  return "abc"
  // CHECK-DIRECTIVE: [[# @LINE-1]]:10-[[# @LINE-1]]:10
}
#sourceLocation()
