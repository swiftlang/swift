// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

//--- header.h

/// This comment contains `markup`.
///
/// - And a list
void testCDecl();

//--- module.modulemap

module MyClangModule { header "header.h" }

//--- test.swift

import MyClangModule

func test() {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):3 %s -- %s -I %t | %FileCheck %s
  testCDecl()
}

// CHECK-LABEL: DOC COMMENT
// CHECK: This comment contains `markup`.
// CHECK: - And a list
// CHECK-LABEL: DOC COMMENT XML
// CHECK: <Function file="{{.*}}" line="9" column="6"><Name>testCDecl</Name><USR>c:@F@testCDecl</USR><Declaration>func testCDecl()</Declaration><Abstract><Para> This comment contains `markup`.</Para></Abstract><Discussion><Para> - And a list</Para></Discussion></Function>
