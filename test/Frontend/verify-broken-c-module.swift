// Tests that `-verify` mode outputs diagnostics from a failing C module
// compilation.
// This needs to be a separate test from verify.swift because compilation will
// terminate after the failing import statement.

// Turn off "pipefail" mode because we intend the `swift` invocation to
// terminate with a non-zero exit code, and we don't want the test to fail
// because of this.
// RUN: set +o pipefail && %target-typecheck-verify-swift -I %S/Inputs/broken-c-module 2>&1 | %FileCheck %s

// CHECK: [[@LINE+3]]:8: error: unexpected error produced: could not build C module 
// CHECK: note: in file included from <module-includes>
// CHECK: broken_c.h:2:18: error: expected ')'
import BrokenCModule
