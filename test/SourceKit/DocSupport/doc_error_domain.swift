// REQUIRES: OS=macosx
// RUN: %sourcekitd-test -req=doc-info -module MyError -- -I %S/Inputs \
// RUN:         %mcp_opt -sdk %sdk | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s

// CHECK: struct MyError {
// CHECK:     enum Code : Int32 {
// CHECK:         case errFirst
// CHECK:         case errSecond
// CHECK:     }
// CHECK:     static var errFirst: MyError.Code { get }
// CHECK:     static var errSecond: MyError.Code { get }

// CHECK:         key.kind: source.lang.swift.decl.struct,
// CHECK-NEXT:    key.name: "MyError",
// CHECK-NEXT:    key.usr: "s:SC7MyErrorV",
// CHECK-NEXT:    This is my cool error code.

// CHECK:             key.kind: source.lang.swift.decl.enum,
// CHECK-NEXT:        key.name: "Code",
// CHECK-NEXT:        key.usr: "c:@E@MyErrorCode",
// CHECK-NEXT:        This is my cool error code.

// CHECK:                 key.kind: source.lang.swift.decl.enumelement,
// CHECK-NEXT:            key.name: "errFirst",
// CHECK-NEXT:            key.usr: "c:@E@MyErrorCode@MyErrFirst",
// CHECK-NEXT:            This is first error.

// CHECK:             key.kind: source.lang.swift.decl.var.static,
// CHECK-NEXT:        key.name: "errFirst",
// CHECK-NEXT:        key.usr: "s:SC7MyErrorV8errFirstAB4CodeOvZ",
// CHECK-NEXT:        This is first error.
