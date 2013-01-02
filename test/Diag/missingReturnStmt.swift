// RUN: %swift %s -i | FileCheck %s
// XFAIL: *
func f() -> (result : Int = 12345) {}  // expected-error {{missing return statement}}
