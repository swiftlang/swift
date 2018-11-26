// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
//
// Ensure that the supplementary output filelist argument is passed to the frontend:
//
// RUN: %swiftc_driver -enable-batch-mode -driver-filelist-threshold=0 -j2 %t/main.swift %t/file-01.swift %t/file-02.swift %t/file-03.swift -o %t/file-01.o -o %t/file-02.o -o %t/file-03.o -### | %FileCheck %s -check-prefix=CHECK-SUPPLEMENTARY-OUTPUT-FILELIST
//
// CHECK-SUPPLEMENTARY-OUTPUT-FILELIST: -supplementary-output-file-map {{.*}}/supplementaryOutputs-
