// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP -import-objc-header %S/Inputs/header.h | FileCheck %s -check-prefix=CHECK-TOP

func foo() {
  #^TOP^#
}

// CHECK-TOP: Decl[FreeFunction]/OtherModule:     doSomethingInHead({#(arg): CInt#})[#Void#]{{$}}
