// REQUIRES: objc_interop
// RUN: rm -rf %t && mkdir -p %t
// RUN: %sourcekitd-test -req=complete -pos=8:5 %s -- %s -module-name Mixed -pch-output-dir %t -import-objc-header %S/Inputs/header.h == -req=complete -pos=8:5 %s -- %s -module-name Mixed -pch-output-dir %t -import-objc-header %S/Inputs/header.h | %FileCheck %s --check-prefix=CHECK-MEMBERS
// RUN: %sourcekitd-test -req=complete -pos=9:1 %s -- %s -module-name Mixed -pch-output-dir %t -import-objc-header %S/Inputs/header.h == -req=complete -pos=9:1 %s -- %s -module-name Mixed -pch-output-dir %t -import-objc-header %S/Inputs/header.h | %FileCheck %s --check-prefix=CHECK-GLOBALS
// RUN: stat %t/*.pch

func foo(x: BaseInHead) {
  x.
}

// CHECK-GLOBALS: doSomethingInHead(:)
// CHECK-GLOBALS: test1(:)
// CHECK-MEMBERS: doIt(:)
