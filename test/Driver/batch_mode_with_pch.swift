// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: touch %t/bridgingHeader.h
//
// RUN: %swiftc_driver -driver-filelist-threshold=0 -enable-batch-mode -num-threads 2 -c %t/main.swift -import-objc-header %t/bridgingHeader.h -o %t/main.o
