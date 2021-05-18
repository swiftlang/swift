// RUN: echo %s > %t.filelist.txt
// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -filelist %t.filelist.txt 2>&1 | %FileCheck %s

// Check that we see the contents of the input file list in the crash log.
// CHECK-LABEL: Stack dump
// CHECK-NEXT: Program arguments: {{.*swift(-frontend)?(c?)(\.exe)?}}
// CHECK-NEXT: Swift version
// CHECK-NEXT: Contents of {{.*}}.filelist.txt:
// CHECK-NEXT: ---
// CHECK-NEXT: test{{[\\/]}}Frontend{{[\\/]}}crash.swift{{$}}
// CHECK-NEXT: ---

// Check that a message when allowing errors is output
// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -experimental-allow-module-with-compiler-errors %s 2>&1 | %FileCheck -check-prefix CHECK-ALLOW %s
// CHECK-ALLOW-LABEL: Stack dump
// CHECK-ALLOW: While allowing modules with compiler errors enabled

func anchor() {}
anchor()

