// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: echo "" > %t/𝔼-file-01.swift 
// RUN: echo "" > %t/😂-file-02.swift 
// RUN: echo "" > %t/Ω-file-03.swift
//
// If the supplementary output file map does not escape the characters in the
// source files, the frontend won't recognize the desired outputs.
//
// RUN: cd %t && %target-build-swift -c -emit-dependencies -serialize-diagnostics -driver-filelist-threshold=0 -j2 main.swift  𝔼-file-01.swift 😂-file-02.swift Ω-file-03.swift -module-name mod
// RUN: %check-file-exists(%/t/main.d)
// RUN: %check-file-exists(%/t/main.dia)
// RUN: %check-file-exists(%/t/main.o)
// RUN: %check-file-exists(%/t/Ω-file-03.d)
// RUN: %check-file-exists(%/t/Ω-file-03.dia)
// RUN: %check-file-exists(%/t/Ω-file-03.o)
// RUN: %check-file-exists(%/t/Ω-file-03.swift)
// RUN: %check-file-exists(%/t/𝔼-file-01.d)
// RUN: %check-file-exists(%/t/𝔼-file-01.dia)
// RUN: %check-file-exists(%/t/𝔼-file-01.o)
// RUN: %check-file-exists(%/t/𝔼-file-01.swift)
// RUN: %check-file-exists(%/t/😂-file-02.d)
// RUN: %check-file-exists(%/t/😂-file-02.dia)
// RUN: %check-file-exists(%/t/😂-file-02.o)
// RUN: %check-file-exists(%/t/😂-file-02.swift)
