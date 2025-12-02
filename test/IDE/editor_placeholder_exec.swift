// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-Xfrontend -warn-on-editor-placeholder) 2> %t/output || true
// RUN: %FileCheck %s < %t/output
// REQUIRES: executable_test

print(<#placeholder#>)
// CHECK: editor_placeholder_exec.swift:[[@LINE-1]]: Fatal error: attempt to evaluate editor placeholder
