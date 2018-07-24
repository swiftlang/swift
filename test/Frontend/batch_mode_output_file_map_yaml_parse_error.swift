// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: (cd %t && not %target-swift-frontend -parse -primary-file main.swift -supplementary-output-file-map %S/Inputs/bad_output_filemap.yaml >%t/errs.txt 2>&1)
// RUN: %FileCheck %s <%t/errs.txt
// CHECK: Unrecognized escape code
// CHECK: Output file map parse failed
