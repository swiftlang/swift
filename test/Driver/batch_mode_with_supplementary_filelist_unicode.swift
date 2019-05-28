// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: echo "" > %t/𝔼-file-01.swift 
// RUN: echo "" > %t/😂-file-02.swift 
// RUN: echo "" > %t/Ω-file-03.swift
//
// If the supplementary output file map does not escape the characters in the
// source files, the frontend won't recognize the desired outputs.
//
// RUN: cd %t && %target-build-swift -c -emit-dependencies -serialize-diagnostics -driver-filelist-threshold=0 -j2 main.swift  @%S/Inputs/unicode-filenames.rsp -module-name mod
//
// All these files should exist and should successfully be deleted
// RUN: rm main.d
// RUN: rm main.dia
// RUN: rm main.o
// RUN: rm Ω-file-03.d
// RUN: rm Ω-file-03.dia
// RUN: rm Ω-file-03.o
// RUN: rm Ω-file-03.swift
// RUN: rm 𝔼-file-01.d
// RUN: rm 𝔼-file-01.dia
// RUN: rm 𝔼-file-01.o
// RUN: rm 𝔼-file-01.swift
// RUN: rm 😂-file-02.d
// RUN: rm 😂-file-02.dia
// RUN: rm 😂-file-02.o
// RUN: rm 😂-file-02.swift
