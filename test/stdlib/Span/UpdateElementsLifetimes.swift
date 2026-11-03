//===--- UpdateElementsLifetimes.swift ------------------------------------===//
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

// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -sil-verify-all \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// The iterator returned by `updateElements(startingAt:copying:)` has a lifetime
// dependency on the source. It should prevent mutations or deinitializations.

// The iterator borrows the array that backs the temporary span.
@available(SwiftStdlib 6.4, *)
func mutateArrayWhileIterating(
  _ destination: inout MutableSpan<Int>, _ source: inout [Int]
) -> Int {
  var index = 0
  var iterator = destination.updateElements(
    startingAt: &index, copying: source.span // expected-note {{conflicting access is here}}
  )
  source.append(1) // expected-error {{overlapping accesses to 'source', but modification requires exclusive access; consider copying to a local variable}}
  return iterator.nextSpan().count
}

// The generic overload: the iterator depends on a borrow of the source.
@available(SwiftStdlib 6.4, *)
func mutateSourceWhileIterating(
  _ destination: inout MutableSpan<Int>, _ source: inout MutableSpan<Int>
) -> Int {
  var index = 0
  var iterator = destination.updateElements(
    startingAt: &index, copying: source // expected-note {{conflicting access is here}}
  )
  source[0] = 99 // expected-error {{overlapping accesses to 'source', but modification requires exclusive access}}
  return iterator.nextSpan().count
}

// The same for an `OutputSpan`: `removeAll()` deinitializes what it reads.
@available(SwiftStdlib 6.4, *)
func emptySourceWhileIterating(
  _ destination: inout MutableSpan<Int>, _ source: inout OutputSpan<Int>
) -> Int {
  var index = 0
  var iterator = destination.updateElements(
    startingAt: &index, copying: source // expected-note {{conflicting access is here}}
  )
  source.removeAll() // expected-error {{overlapping accesses to 'source', but modification requires exclusive access}}
  return iterator.nextSpan().count
}

// `source` is destroyed on return, so the iterator can't escape.
@available(SwiftStdlib 6.4, *)
@_lifetime(immortal)
func iteratorOutlivesSource(
  _ destination: inout MutableSpan<Int>
) -> UniqueArray<Int>.BorrowingIterator {
  var index = 0
  let source = UniqueArray<Int>(copying: 0..<4)
  return destination.updateElements( // expected-error {{lifetime-dependent value escapes its scope}} expected-note {{this use causes the lifetime-dependent value to escape}}
    startingAt: &index, copying: source // expected-note {{it depends on this scoped access to variable 'source'}}
  )
}
