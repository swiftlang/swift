//===--- SpanUsageHints.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

func testSpan<T>(_ span: Span<T>) {
  _ = span[0..<3]        // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[0...3]        // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[...]          // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[..<3]         // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[3...]         // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span.prefix(5)     // expected-error {{has been renamed to 'extracting(first:)'}}
  _ = span.suffix(5)     // expected-error {{has been renamed to 'extracting(last:)'}}
  _ = span.dropFirst(5)  // expected-error {{has been renamed to 'extracting(droppingFirst:)'}}
  _ = span.dropLast(5)   // expected-error {{has been renamed to 'extracting(droppingLast:)'}}
}

func testRawSpan(_ span: RawSpan) {
  _ = span[0..<3]        // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[0...3]        // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[...]          // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[..<3]         // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span[3...]         // expected-error {{has been renamed to 'extracting(_:)'}}
  _ = span.prefix(5)     // expected-error {{has been renamed to 'extracting(first:)'}}
  _ = span.suffix(5)     // expected-error {{has been renamed to 'extracting(last:)'}}
  _ = span.dropFirst(5)  // expected-error {{has been renamed to 'extracting(droppingFirst:)'}}
  _ = span.dropLast(5)   // expected-error {{has been renamed to 'extracting(droppingLast:)'}}
}
