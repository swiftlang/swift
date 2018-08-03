//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


import StdlibUnittest

// Generate two overloads: one for Array (which will get
// picked up when the caller passes a literal), and another that
// accepts any appropriate Collection type.

public func checkIterator<
  Expected: Collection, I : IteratorProtocol
>(
  _ expected: Expected,
  _ iterator: I,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
  sameValue: (Expected.Element, Expected.Element) -> Bool
) where I.Element == Expected.Element {
  // Copying a `IteratorProtocol` is allowed.
  var mutableGen = iterator
  var actual: [Expected.Element] = []
  while let e = mutableGen.next() {
    actual.append(e)
  }
  expectEqualSequence(
    expected, actual, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), sameValue: sameValue)

  // Having returned `.None` once, a `IteratorProtocol` should not generate
  // more elements.
  for _ in 0..<10 {
    expectNil(mutableGen.next(), message(),
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
  }
}

public func checkIterator<
  Expected: Collection, I : IteratorProtocol
>(
  _ expected: Expected,
  _ iterator: I,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all
) where I.Element == Expected.Element, Expected.Element : Equatable {
  checkIterator(
    expected, iterator, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), showFrame: false,
    resiliencyChecks: resiliencyChecks
  ) { $0 == $1 }
}

public func checkSequence<
  Expected: Collection, S : Sequence
>(
  _ expected: Expected,
  _ sequence: S,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
  sameValue: (Expected.Element, Expected.Element) -> Bool
) where S.Element == Expected.Element {
  let expectedCount: Int = numericCast(expected.count)
  checkIterator(
    expected, sequence.makeIterator(), message(),
  stackTrace: stackTrace.pushIf(showFrame, file: file, line: line),
    resiliencyChecks: resiliencyChecks,
    sameValue: sameValue)

  expectGE(
    expectedCount, sequence.underestimatedCount, message(),
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))

  // Test `_copyContents(initializing:)` if we can do so without destroying the
  // sequence.
  _ = sequence._preprocessingPass { () -> Void in
    var count = 0
    for _ in sequence { count += 1 }
    let ptr = UnsafeMutablePointer<S.Element>.allocate(capacity: count)
    let buf = UnsafeMutableBufferPointer(start: ptr, count: count)
    var (remainders,writtenUpTo) = sequence._copyContents(initializing: buf)
    expectTrue(remainders.next() == nil,
      "_copyContents returned unwritten elements")
    expectTrue(writtenUpTo == buf.endIndex,
      "_copyContents failed to use entire buffer")
    expectEqualSequence(expected, buf, message(),
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), sameValue: sameValue)
    ptr.deinitialize(count: count)
    ptr.deallocate()
  }

  // Test `_copyToContiguousArray()` if we can do so
  // without destroying the sequence.
  _ = sequence._preprocessingPass { () -> Void in
    let copy = sequence._copyToContiguousArray()
    expectEqualSequence(expected, copy, message(),
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), sameValue: sameValue)
  }
}

public func checkSequence<
  Expected: Collection, S : Sequence
>(
  _ expected: Expected,
  _ sequence: S,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all
) where
  S.Element == Expected.Element,
  S.Element : Equatable {

  checkSequence(
    expected, sequence, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), showFrame: false,
    resiliencyChecks: resiliencyChecks
  ) { $0 == $1 }
}

public func checkIterator<
  Element, I : IteratorProtocol
>(
  _ expected: Array<Element>,
  _ iterator: I,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
  sameValue: (Element, Element) -> Bool
) where I.Element == Element {
  // Copying a `IteratorProtocol` is allowed.
  var mutableGen = iterator
  var actual: [Element] = []
  while let e = mutableGen.next() {
    actual.append(e)
  }
  expectEqualSequence(
    expected, actual, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), sameValue: sameValue)

  // Having returned `.None` once, a `IteratorProtocol` should not generate
  // more elements.
  for _ in 0..<10 {
    expectNil(mutableGen.next(), message(),
  stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
  }
}

public func checkIterator<
  Element, I : IteratorProtocol
>(
  _ expected: Array<Element>,
  _ iterator: I,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all
) where I.Element == Element, Element : Equatable {
  checkIterator(
    expected, iterator, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), showFrame: false,
    resiliencyChecks: resiliencyChecks
  ) { $0 == $1 }
}

public func checkSequence<
  Element, S : Sequence
>(
  _ expected: Array<Element>,
  _ sequence: S,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
  sameValue: (Element, Element) -> Bool
) where S.Element == Element {
  let expectedCount: Int = numericCast(expected.count)
  checkIterator(
    expected, sequence.makeIterator(), message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line),
    resiliencyChecks: resiliencyChecks,
    sameValue: sameValue)

  expectGE(
    expectedCount, sequence.underestimatedCount, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))

  // Test `_copyContents(initializing:)` if we can do so without destroying the
  // sequence.
  _ = sequence._preprocessingPass { () -> Void in
    var count = 0
    for _ in sequence { count += 1 }
    let ptr = UnsafeMutablePointer<S.Element>.allocate(capacity: count)
    let buf = UnsafeMutableBufferPointer(start: ptr, count: count)
    var (remainders,writtenUpTo) = sequence._copyContents(initializing: buf)
    expectTrue(remainders.next() == nil,
      "_copyContents returned unwritten elements")
    expectTrue(writtenUpTo == buf.endIndex,
      "_copyContents failed to use entire buffer")
    expectEqualSequence(expected, buf, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), sameValue: sameValue)
    ptr.deinitialize(count: count)
    ptr.deallocate()
  }

  // Test `_copyToContiguousArray()` if we can do so
  // without destroying the sequence.
  _ = sequence._preprocessingPass { () -> Void in
    let copy = sequence._copyToContiguousArray()
    expectEqualSequence(expected, copy, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), sameValue: sameValue)
  }
}

public func checkSequence<
  Element, S : Sequence
>(
  _ expected: Array<Element>,
  _ sequence: S,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line,
  resiliencyChecks: CollectionMisuseResiliencyChecks = .all
) where
  S.Element == Element,
  S.Element : Equatable {

  checkSequence(
    expected, sequence, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line), showFrame: false,
    resiliencyChecks: resiliencyChecks
  ) { $0 == $1 }
}

