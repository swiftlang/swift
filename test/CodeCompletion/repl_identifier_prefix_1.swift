// RUN: %swift-ide-test -repl-code-completion -source-filename %s | FileCheck %s

// CHECK: Begin completions
// CHECK-NEXT: SwiftDecl: true[#Bool#]{{$}}
// CHECK-NEXT: End completions

tru
