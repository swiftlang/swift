// Tests that `-verify` mode outputs diagnostics from a failing C module
// compilation.
// This needs to be a separate test from verify.swift because compilation will
// terminate after the failing import statement.

// RUN: not %target-typecheck-verify-swift -I %S/Inputs/broken-c-module 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not note: --implicit-check-not warning:

// CHECK: <unknown>:0: error: fatal error encountered while in -verify mode
// CHECK: [[@LINE+5]]:8: error: unexpected error produced: could not build
// CHECK: error: unexpected note produced: in file included from <module-includes>:1:
// CHECK: note: file '<module-includes>' is not parsed for 'expected' statements. Use '-verify-additional-file <module-includes>' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file
// CHECK: error: unexpected error produced: expected function body after function declarator
// CHECK: note: file '{{.*}}broken_c.h' is not parsed for 'expected' statements. Use '-verify-additional-file {{.*}}broken_c.h' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file
import BrokenCModule
