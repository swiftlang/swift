// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Test that update-verify-tests correctly handles expansion blocks from a
// different verify prefix. When updating with "--prefix diagnose-", expansion
// blocks using the "macro-" prefix (// expected-macro-expansion{{ ... // }})
// must be parsed for structural context so that the closing // }} markers are
// not mistaken for orphaned closers, which intentionally throws an error.

// RUN: not %target-swift-frontend-verify -verify-additional-prefix diagnose- -typecheck %t/test.swift -Rmodule-api-import 2>%t/output.txt
// RUN: %update-verify-tests --prefix diagnose- < %t/output.txt
// RUN: %target-swift-frontend-verify -verify-additional-prefix diagnose- -typecheck %t/test.swift -Rmodule-api-import
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
// expected-macro-expansion@+3:1{{
//   expected-macro-remark@1{{struct 'String' is imported via 'Swift'}}
// }}
public typealias Foo = String
//--- test.swift.expected
// expected-diagnose-remark@+4{{struct 'String' is imported via 'Swift'}}
// expected-macro-expansion@+3:1{{
//   expected-macro-remark@1{{struct 'String' is imported via 'Swift'}}
// }}
public typealias Foo = String
