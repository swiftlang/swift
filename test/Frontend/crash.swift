// RUN: %empty-directory(%t)
// RUN: echo %s > %t/primary.filelist.txt
// RUN: echo "" > %t/empty.swift
// RUN: echo "%t/empty.swift" > %t/all.filelist.txt
// RUN: echo %s >> %t/all.filelist.txt

// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -primary-filelist %t/primary.filelist.txt -filelist %t/all.filelist.txt 2>&1 | %FileCheck %s

// Check that we see the contents of the input file list in the crash log.
// CHECK-NOT: while allowing modules with compiler errors
// CHECK-LABEL: Stack dump
// CHECK-NEXT: Program arguments: {{.*swift(-frontend)?(c?)(\.exe)?}}
// CHECK-NEXT: Swift version
// CHECK-NEXT: Compiling with effective version
// CHECK-NEXT: Contents of {{.*}}/all.filelist.txt:
// CHECK-NEXT: ---
// CHECK-NEXT: {{[\\/]}}empty.swift{{$}}
// CHECK-NEXT: {{[\\/]}}crash.swift{{$}}
// CHECK-NEXT: ---
// CHECK-NEXT: Contents of {{.*}}/primary.filelist.txt:
// CHECK-NEXT: ---
// CHECK-NEXT: {{[\\/]}}crash.swift{{$}}
// CHECK-NEXT: ---

// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -experimental-allow-module-with-compiler-errors %s 2>&1 | %FileCheck -check-prefix CHECK-ALLOW %s
// CHECK-ALLOW: Program arguments: {{.*}} -experimental-allow-module-with-compiler-errors
// CHECK-ALLOW: Compiling with effective version {{.*}} while allowing modules with compiler errors

// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -experimental-allow-module-with-compiler-errors -swift-version 5 %s 2>&1 | %FileCheck -check-prefix CHECK-CURRENT %s
// CHECK-CURRENT: Program arguments: {{.*}} -experimental-allow-module-with-compiler-errors
// CHECK-CURRENT: Compiling with effective version {{.*}} while allowing modules with compiler errors

func anchor() {}
anchor()
