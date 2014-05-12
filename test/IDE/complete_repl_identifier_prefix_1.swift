// RUN: %swift-ide-test -repl-code-completion -source-filename %s | FileCheck %s

// CHECK: Begin completions
// CHECK-NEXT: Decl[GlobalVar]/OtherModule: true[#Bool#]{{$}}
// CHECK-NEXT: End completions

tru
