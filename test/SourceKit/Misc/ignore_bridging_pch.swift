// RUN: %sourcekitd-test -req=complete -pos=5:3 %s -- -enable-bridging-pch %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=5:3 %s -- -disable-bridging-pch %s | %FileCheck %s

var s = 10
s.

// CHECK: littleEndian
