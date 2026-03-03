// RUN: %empty-directory(%t)
// RUN: echo %s > %t/filelist.txt
// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -filelist %t/filelist.txt 2>&1 | %FileCheck %s

// CHECK-LABEL: Stack dump
// CHECK-NEXT: Program arguments: {{.*swift(-frontend)?(c?)(\.exe)?}}
// CHECK-NEXT: Swift version
// CHECK-NEXT: Compiling with effective version

// Filelist contents should be omitted since this is a whole-module compile.
// CHECK-NOT:  Contents of

func anchor() {}
anchor()
