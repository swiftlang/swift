// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -primary-file %t/a.swift %t/b.swift -debug-forbid-typecheck-prefix NOTYPECHECK -enable-upcoming-feature IsolatedDefaultValues 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_IsolatedDefaultValues

//--- a.swift
func f() {
  _ = S()
}

//--- b.swift
struct S {
  var x: Int = NOTYPECHECK_x
}

// CHECK: forbidden typecheck occurred: NOTYPECHECK_x
