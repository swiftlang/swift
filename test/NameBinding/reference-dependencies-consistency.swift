// Some types, such as StringLiteralType, used to be cached in the TypeChecker.
// Consequently, the second primary file (in batch mode) to use that type would
// hit in the cache and no dependency would be recorded.
// This test ensures that this bug stays fixed.
//
// RUN: %empty-directory(%t)
//
// Create two identical inputs, each needing StringLiteralType:
//
// RUN: echo 'fileprivate var v: String { return "\(x)" }; fileprivate let x = "a"' >%t/1.swift
// RUN: echo 'fileprivate var v: String { return "\(x)" }; fileprivate let x = "a"' >%t/2.swift
//
// RUN:  %target-swift-frontend -typecheck -primary-file %t/1.swift -primary-file %t/2.swift -emit-reference-dependencies-path %t/1.swiftdeps -emit-reference-dependencies-path %t/2.swiftdeps
//
// RUN cmp -s %t/1.swiftdeps %t/2.swiftdeps
