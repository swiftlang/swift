// RUN: not %swift  -c -primary-file a.swift b.swift -emit-module-path one -emit-module-path two 2>&1 | %FileCheck %s -check-prefix=CHECK1
// CHECK1: <unknown>:0: error: wrong number of '-emit-module-path' arguments (expected 1, got 2)
