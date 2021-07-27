// RUN: echo %s > %t.filelist.txt
// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -filelist %t.filelist.txt 2>&1 | %FileCheck %s

// Check that we see the contents of the input file list in the crash log.
// CHECK-NOT: Compiling with {{.*}} while allowing modules with compiler errors enabled
// CHECK-LABEL: Stack dump
// CHECK-NEXT: Program arguments: {{.*swift(-frontend)?(c?)(\.exe)?}}
// CHECK-NEXT: Swift version
// CHECK-NEXT: Compiling with effective version
// CHECK-NEXT: Contents of {{.*}}.filelist.txt:
// CHECK-NEXT: ---
// CHECK-NEXT: test{{[\\/]}}Frontend{{[\\/]}}crash.swift{{$}}
// CHECK-NEXT: ---

// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -experimental-allow-module-with-compiler-errors %s 2>&1 | %FileCheck -check-prefix CHECK-ALLOW %s
// CHECK-ALLOW: Program arguments: {{.*}} -experimental-allow-module-with-compiler-errors
// CHECK-ALLOW: Compiling with effective version

// RUN: not --crash %target-swift-frontend -typecheck -debug-crash-after-parse -experimental-allow-module-with-compiler-errors -swift-version 5 %s 2>&1 | %FileCheck -check-prefix CHECK-CURRENT %s
// CHECK-CURRENT: Program arguments: {{.*}} -experimental-allow-module-with-compiler-errors
// CHECK-CURRENT: Compiling with the current language version

func anchor() {}
anchor()

