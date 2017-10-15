// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TOP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TYPE
// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -code-completion -pch-output-dir %t -source-filename %s -code-completion-token=TOP -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TOP
// RUN: %target-swift-ide-test -code-completion -pch-output-dir %t -source-filename %s -code-completion-token=TYPE -import-objc-header %S/Inputs/header.h | %FileCheck %s -check-prefix=CHECK-TYPE
// RUN: stat %t/*.pch
// RUN: cp %S/Inputs/header.h %t
// RUN: %target-swift-ide-test -code-completion -pch-output-dir %t/pch -source-filename %s -code-completion-token=TOP -import-objc-header %t/header.h | %FileCheck %s -check-prefix=CHECK-TOP
// RUN: stat %t/pch/*.pch
// RUN: echo '// new stuff' >> %t/header.h
// RUN: %target-swift-ide-test -code-completion -pch-output-dir %t/pch -source-filename %s -code-completion-token=TOP -import-objc-header %t/header.h | %FileCheck %s -check-prefix=CHECK-TOP

// Check that code-completion functions if there is an error in the header.
// FIXME: Nothing from the bridging header gets imported if there is an error, we should be more resilient.
// RUN: cp %S/Inputs/header.h %t/header-with-error.h
// RUN: echo '#error error in header' >> %t/header-with-error.h
// RUN: %target-swift-ide-test -code-completion -pch-output-dir %t/pch -source-filename %s -code-completion-token=TOP -import-objc-header %t/header-with-error.h | %FileCheck %s -check-prefix=CHECK-WITH-ERROR
// CHECK-WITH-ERROR: Decl[FreeFunction]{{.*}} foo()

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
