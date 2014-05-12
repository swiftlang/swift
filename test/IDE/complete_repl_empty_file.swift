// RUN: %swift-ide-test -repl-code-completion -source-filename %s | FileCheck %s

// CHECK: Begin completions
// CHECK-DAG: Decl[GlobalVar]/OtherModule: true[#Bool#]{{$}}
// CHECK: End completions

