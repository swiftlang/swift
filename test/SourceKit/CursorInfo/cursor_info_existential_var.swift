protocol MyError {
    var myVar: String { get }
}

func foo(error: MyError) {
    _ = error.myVar
}

// RUN: %sourcekitd-test -req=cursor -pos=6:15 %s -- %s | %FileCheck %s
// CHECK: myVar
// CHECK-NEXT: s:27cursor_info_existential_var7MyErrorP5myVarSSvp
// CHECK-NEXT: source.lang.swift
// CHECK-NEXT: String
