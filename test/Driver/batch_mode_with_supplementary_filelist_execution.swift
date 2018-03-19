// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: touch %t/file-01.swift %t/file-02.swift %t/Ωfile-03.swift
//
// Ensure that the supplementary output filelist argument is passed to the frontend:
//
// RUN: %target-build-swift -save-temps -enable-batch-mode -driver-use-filelists -j2 %t/main.swift  %t/file-01.swift %t/file-02.swift %t/Ωfile-03.swift  -o %t/a.out 2> %t/shouldBeEmpty
// Check for errors such as "Unrecognized escape code".
// RUN: test -z "`cat %t/shouldBeEmpty`"
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-HELLO-WORLD
// CHECK-HELLO-WORLD: Hello, World!
