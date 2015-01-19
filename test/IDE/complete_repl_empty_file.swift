// RUN: %target-swift-ide-test -repl-code-completion -source-filename %s | FileCheck %s

// CHECK: Begin completions
// CHECK-DAG: {{^}}true: Bool{{$}}
// CHECK: End completions

