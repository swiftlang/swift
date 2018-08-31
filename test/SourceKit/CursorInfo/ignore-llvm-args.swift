_ = ""

// rdar://problem/38314383
// RUN: %sourcekitd-test -req=cursor -offset=0 %s -- -Xllvm -aarch64-use-tbi %s | %FileCheck %s

// CHECK: <empty cursor info>
