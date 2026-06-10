// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/other.swift -primary-file %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/other.swift %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/other.swift -primary-file %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/main.swift -primary-file %t/other.swift
// RUN: %target-swift-frontend -typecheck -verify %t/other.swift %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift %t/other.swift

// Make sure we don't crash when lazily type-checking 'y' from other.

//--- main.swift
var x: Int?
guard let x else { fatalError() }
let y = x

//--- other.swift
let z = y
