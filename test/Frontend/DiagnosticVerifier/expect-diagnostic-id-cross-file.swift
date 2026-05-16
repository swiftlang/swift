// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/file1.swift %t/file2.swift

//--- file1.swift
let badInit = // #marker1
// expected-error@#marker2 [cannot_convert_initializer_value]

//--- file2.swift
// expected-error@#marker1 [expected_init_value]
let typeMismatch: Double = "" // #marker2