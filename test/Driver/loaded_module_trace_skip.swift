// rdar://problem/54860311.
// RUN: not %target-typecheck-verify-swift -emit-loaded-module-trace -o %t/mytrace -I %S/Inputs 2>&1 | %FileCheck %s

import IllformedModule
// CHECK: unexpected error produced: malformed compiled module
