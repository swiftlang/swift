// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: touch %t/𝔼-file-01.swift %t/😂-file-02.swift %t/Ω-file-03.swift
//
// Ensure that the supplementary output filelist argument is passed to the frontend.
// Also use some characters outside the BMP.
//
// RUN: %target-build-swift -save-temps -emit-dependencies -emit-loaded-module-trace -emit-loaded-module-trace-path %t/lmt -serialize-diagnostics -driver-use-filelists -j2 %t/main.swift  %t/𝔼-file-01.swift %t/😂-file-02.swift %t/Ω-file-03.swift  -o %t/a.out 2> %t/shouldBeEmpty
// RUN: test -z "`cat %t/shouldBeEmpty`"
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-HELLO-WORLD
// CHECK-HELLO-WORLD: Hello, World!
