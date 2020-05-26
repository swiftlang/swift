// RUN: echo %s > %t.filelist.txt
// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -filelist %t.filelist.txt 2>&1 | %FileCheck %s

// Check that we see the contents of the input file list in the crash log.
// CHECK-LABEL: Stack dump
// CHECK-NEXT: Program arguments: {{.*swift(c?)(\.exe)?}} -frontend
// CHECK-NEXT: Swift version
// CHECK-NEXT: Contents of {{.*}}.filelist.txt:
// CHECK-NEXT: ---
// CHECK-NEXT: test{{[\\/]}}Frontend{{[\\/]}}crash.swift{{$}}
// CHECK-NEXT: ---

func anchor() {}
anchor()

