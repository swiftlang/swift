// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t.o -typecheck %s
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s

// CHECK: record-comments.swift
// CHECK: ------------
// CHECK: comment-tag/Swift | <no-name> | t:this_is_a_tag | <no-cgname> | Def -
// CHECK: comment-tag/Swift | <no-name> | t:and_another | <no-cgname> | Def -
// CHECK: ------------

/// Hello
/// - Tag: this_is_a_tag
/// - Tag: and_another
func foo() {}
// CHECK: [[@LINE-3]]:5 | comment-tag/Swift | t:this_is_a_tag | Def | rel: 0
// CHECK: [[@LINE-3]]:5 | comment-tag/Swift | t:and_another | Def | rel: 0
// CHECK: [[@LINE-3]]:6 | function/Swift |
