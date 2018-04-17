var s = 10
s.

// CHECK: littleEndian

// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -j4 %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -j 4 %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -v %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -c %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -embed-bitcode %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -enable-bridging-pch %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -disable-bridging-pch %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -verify-debug-info %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -Xlinker blah %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -use-ld=blah %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -incremental %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -driver-time-compilation %s | %FileCheck %s


// Mode flags
// RUN: %empty-directory(%t)
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -emit-object %s -o %t/test.o | %FileCheck %s
// RUN: not find %t/test.o
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -emit-executable %s -o %t/test | %FileCheck %s
// RUN: not find %t/test
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -emit-library -module-name test %s -o %t/test | %FileCheck %s
// RUN: not find %t/test
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -emit-module -module-name test %s  -o %t/test.swiftmodule | %FileCheck %s
// RUN: not find %t/test.swiftmodule
