// RUN: %target-typecheck-verify-swift %s -I %S/Inputs/custom-modules -enable-experimental-feature ImportMacroAliases
// XFAIL: *

// REQUIRES: swift_feature_ImportMacroAliases

// expected-no-diagnostics

import RetroactiveVersioning
let _ = kVersion
