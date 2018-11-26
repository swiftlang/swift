// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: touch %t/bridgingHeader.h
//
// Make sure the proper warning is emitted:
//
// RUN: %swiftc_driver -enable-batch-mode -num-threads 2 %t/main.swift -import-objc-header %t/bridgingHeader.h -### 2>&1 | %FileCheck %s
//
// CHECK: ignoring -num-threads argument; cannot multithread batch mode
//
// Make sure that it actually works. (The link step fails if -num-threads is not ignored.)
//
// RUN: %swiftc_driver -enable-batch-mode -num-threads 2 %t/main.swift -import-objc-header %t/bridgingHeader.h
