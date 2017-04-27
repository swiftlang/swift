// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TOP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TYPE
// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-ide-test -code-completion -pch-output-dir %t -source-filename %s -code-completion-token=TOP -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TOP
// RUN: %target-swift-ide-test -code-completion -pch-output-dir %t -source-filename %s -code-completion-token=TYPE -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TYPE
// RUN: stat %t/*.pch

// REQUIRES: objc_interop

func foo() {
  #^TOP^#
  // CHECK-TOP-NOT: function_as_swift_private
  // CHECK-TOP: Decl[FreeFunction]/OtherModule[__ObjC]:     doSomethingInHead({#(arg): Int32#})[#Void#]{{; name=.+$}}
}

func bar() {
  let _: #^TYPE^#
  // CHECK-TYPE-DAG: Decl[Class]/OtherModule[__ObjC]:    SameName[#SameName#]{{; name=.+$}}
  // CHECK-TYPE-DAG: Decl[Protocol]/OtherModule[__ObjC]: SameNameProtocol[#SameNameProtocol#]{{; name=.+$}}
}
