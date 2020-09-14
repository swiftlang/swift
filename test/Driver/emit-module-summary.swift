// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: cd %t

// RUN: %target-swiftc_driver -emit-sib -emit-module-summary -emit-module-summary-path %t/main.swiftmodulesummary %t/main.swift
// RUN: test -f %t/main.swiftmodulesummary
