// RUN: %target-repl-run-simple-swift -enable-verify-exclusivity -enforce-exclusivity=checked | %FileCheck %s
// REQUIRES: asserts
// REQUIRES: OS=macosx
// REQUIRES: swift_repl

// Test the combination of SILGen + DiagnoseStaticExclusivity with verification.
//
// This augments access_marker_verify with tests that require the repl.

// Global variable initialization is different in the repl.
// CHECK: glob_i8 : Int8 = 8
var glob_i8:   Int8 = 8
