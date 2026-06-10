// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-Xfrontend -warn-on-editor-placeholder) 2> %t/output || true
// RUN: %FileCheck %s < %t/output
// REQUIRES: executable_test

// The runtime error format changed after the 5.3 release.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

print(<#placeholder#>)
// CHECK: editor_placeholder_exec.swift:[[@LINE-1]]: Fatal error: attempt to evaluate editor placeholder
