// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s

// level 512.
// The C++ parser and SwiftParser report the limit with different wording, at
// slightly different columns.
// CHECK: :[[@LINE+1]]:{{256|258}}: error: {{structure nesting level exceeded maximum of 256|parsing has exceeded the maximum nesting level}}
[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[({[(


;blah blah blah

// Note: extra newlines below ensure that context printing doesn't show the
// lines that we shouldn't see.



// CHECK-NOT: ';'
