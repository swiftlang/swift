// RUN: not %swift 2>&1 | %FileCheck %s -check-prefix=CHECK1
// RUN: not %swift %s 2>&1 | %FileCheck %s -check-prefix=CHECK1
// RUN: not %swift -typecheck 2>&1 | %FileCheck %s -check-prefix=CHECK2
// RUN: not %swift -emit-sil 2>&1 | %FileCheck %s -check-prefix=CHECK2
// RUN: not %swift -emit-object 2>&1 | %FileCheck %s -check-prefix=CHECK2

// CHECK1: <unknown>:0: error: no frontend action was selected
// CHECK2: <unknown>:0: error: this mode requires at least one input file
