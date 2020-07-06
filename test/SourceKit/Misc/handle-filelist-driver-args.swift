let x = 10
x.littleEndian

// RUN: %empty-directory(%t)
// RUN: echo %s > %t/tmp.SwiftFileList
// RUN: %target-swiftc_driver -typecheck @%t/tmp.SwiftFileList
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- @%t/tmp.SwiftFileList | %FileCheck %s -check-prefix=COMPLETE
// COMPLETE: littleEndian
