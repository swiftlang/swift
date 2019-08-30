// RUN: not %target-typecheck-verify-swift -emit-loaded-module-trace -o %t/mytrace -I %S/Inputs 2>&1 | %FileCheck %s
// XFAIL: *

import IllformedModule
// CHECK: unexpected error produced: malformed compiled module
