_ = ""

// rdar://problem/38314383
// RUN: not %sourcekitd-test 2>&1 -req=cursor -offset=0 %s -- -Xllvm -aarch64-use-tbi %s | %FileCheck %s

// CHECK: (Request Failed): Resolved to incomplete expression or statement.
