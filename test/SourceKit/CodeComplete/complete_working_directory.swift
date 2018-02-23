import Foo

// REQUIRES: objc_interop

// RUN: %sourcekitd-test -req=complete.open -pos=2:1 -req-opts=hidelowpriority=0 %s -- %s -F libIDE-mock-sdk -working-directory %S/../Inputs | %FileCheck %s
// RUN: %sourcekitd-test -req=complete.open -pos=2:1 -req-opts=hidelowpriority=0 %s -- %s -Xcc -F -Xcc libIDE-mock-sdk -working-directory %S/../Inputs | %FileCheck %s

// CHECK: fooFunc
