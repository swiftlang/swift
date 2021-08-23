// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift
// RUN: touch %t/file-02.swift
// RUN: cd %t
// RUN: %target-swift-frontend -emit-module -primary-file file-01.swift -primary-file file-02.swift -o file-01.swiftmodule -o file-02.swiftmodule -module-name foo -emit-loaded-module-trace-path=%t/trace.json
