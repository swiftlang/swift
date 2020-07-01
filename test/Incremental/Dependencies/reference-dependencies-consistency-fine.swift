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
// RUN:  %target-swift-frontend -typecheck -disable-direct-intramodule-dependencies -primary-file %t/1.swift -primary-file %t/2.swift -emit-reference-dependencies-path %t/1.swiftdeps -emit-reference-dependencies-path %t/2.swiftdeps
//
// Sequence numbers can vary
// RUN: sed -e 's/[0-9][0-9]*/N/g' -e 's/N, //g' -e '/^ *$/d' <%t/1.swiftdeps | sort >%t/1-processed.swiftdeps
// RUN: sed -e 's/[0-9][0-9]*/N/g' -e 's/N, //g' -e '/^ *$/d' <%t/2.swiftdeps | sort >%t/2-processed.swiftdeps

// RUN: cmp -s %t/1-processed.swiftdeps %t/2-processed.swiftdeps
