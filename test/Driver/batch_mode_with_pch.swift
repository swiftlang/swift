// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: touch %t/bridgingHeader.h
//
// RUN: %swiftc_driver -driver-use-filelists -enable-batch-mode -num-threads 2 %t/main.swift -import-objc-header %t/bridgingHeader.h
