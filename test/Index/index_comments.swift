// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

/// Hello
/// - Tag: this_is_a_tag
/// - Tag: and_another
func foo() {}
// CHECK: [[@LINE-3]]:5 | comment-tag/Swift |  | t:this_is_a_tag | Def | rel: 0
// CHECK: [[@LINE-3]]:5 | comment-tag/Swift |  | t:and_another | Def | rel: 0
// CHECK: [[@LINE-3]]:6 | function/Swift |
