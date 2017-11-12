var s = 10
s.

// CHECK: littleEndian

// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -j4 %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -c %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -v %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -embed-bitcode %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -enable-bridging-pch %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -disable-bridging-pch %s | %FileCheck %s
