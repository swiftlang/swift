// RUN: cat %s | tr '\132' '\0' > %t
// RUN: %swift-syntax-test -input-source-filename %t -dump-full-tokens 2>&1 >/dev/null | %FileCheck %t

// CHECK: 5:18: warning: nul character embedded in middle of file
let a = 3 // nul(Z)
