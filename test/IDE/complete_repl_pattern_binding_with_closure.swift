// RUN: %target-swift-ide-test -repl-code-completion -source-filename %s | %FileCheck %s
// CHECK: Begin completions
// CHECK: print
// CHECK: End completions
let (a, b) = ({ _ in (1, 2) })(0)
p
