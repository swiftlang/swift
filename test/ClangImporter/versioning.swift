// RUN: %target-typecheck-verify-swift %s -I %S/Inputs/custom-modules
// XFAIL: *

// expected-no-diagnostics

import RetroactiveVersioning
let _ = kVersion
