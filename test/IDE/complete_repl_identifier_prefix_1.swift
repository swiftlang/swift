// RUN: %target-swift-ide-test -repl-code-completion -source-filename %s | FileCheck %s

// CHECK: Begin completions
// CHECK-NEXT: {{^}}true: Bool{{$}}
// CHECK-NEXT: End completions

tru
