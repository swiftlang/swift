// Tests that `-verify` mode outputs diagnostics from a failing C module
// compilation.
// This needs to be a separate test from verify.swift because compilation will
// terminate after the failing import statement.

// RUN: not %target-typecheck-verify-swift -I %S/Inputs/broken-c-module 2>&1 | %FileCheck %s

// CHECK: [[@LINE+3]]:8: error: unexpected error produced: could not build
// CHECK: note: diagnostic produced elsewhere: in file included from <module-includes>
// CHECK: broken_c.h:2:11: error: diagnostic produced elsewhere: expected function body after function declarator
import BrokenCModule
