// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: touch %t/𝔼-file-01.swift %t/😂-file-02.swift %t/Ω-file-03.swift
//
// Ensure that the supplementary output filelist argument is passed to the frontend.
// Also use some characters outside the BMP.
//
// RUN: %target-build-swift -emit-dependencies -serialize-diagnostics -driver-filelist-threshold=0 -j2 %t/main.swift  %t/𝔼-file-01.swift %t/😂-file-02.swift %t/Ω-file-03.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-HELLO-WORLD
// CHECK-HELLO-WORLD: Hello, World!
