// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck -primary-file %t/file2.swift %t/file1.swift -verify

//--- file1.swift

typealias Bar = [Int]

//--- file2.swift

func foo() -> Bar { [] }
