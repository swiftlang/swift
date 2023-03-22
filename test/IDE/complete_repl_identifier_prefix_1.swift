// RUN: %target-swift-ide-test -repl-code-completion -source-filename %s | %FileCheck %s

// CHECK-DAG: .self: _
// CHECK-DAG: {{^}}true: Bool{{$}}

tru
