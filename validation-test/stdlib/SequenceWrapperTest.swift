//===--- SequenceWrapperTest.swift ----------------------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
import StdlibUnittest
import StdlibCollectionUnittest


struct BasicSequenceWrapper<
  Base_: Sequence
> : _SequenceWrapper, Sequence {
  var _base: Base_
}

var sequenceWrapperTests = TestSuite("SequenceWrapper")

let base = LoggingSequence(wrapping: [OpaqueValue(1)])
let direct = BasicSequenceWrapper(_base: base)
let indirect = LoggingSequence(wrapping: direct)
let dispatchLog = base.log

func expectWrapperDispatch<R1, R2>(
  _ directOperation: @autoclosure () -> R1,
  _ indirectOperation: @autoclosure () -> R2,
  _ counters: TypeIndexed<Int>,
  //===--- TRACE boilerplate ----------------------------------------------===//
  _ message: @autoclosure () -> String = "",
  showFrame: Bool = true,
  stackTrace: SourceLocStack = SourceLocStack(),  
  file: String = #file, line: UInt = #line
) {
  let newTrace = stackTrace.pushIf(showFrame, file: file, line: line)
  counters.reset()
  _ = directOperation()
  expectEqual([base.selfType: 1], counters, message(), stackTrace: newTrace)
  counters.reset()
  _ = indirectOperation()
  expectEqual(
    [base.selfType: 1, indirect.selfType: 1], counters,
    message(), stackTrace: newTrace)
}

sequenceWrapperTests.test("Dispatch/makeIterator()") {
  expectWrapperDispatch(
    direct.makeIterator(), indirect.makeIterator(), dispatchLog.makeIterator)
}

sequenceWrapperTests.test("Dispatch/underestimatedCount") {
  expectWrapperDispatch(
    direct.underestimatedCount, indirect.underestimatedCount,
    dispatchLog.underestimatedCount)
}

sequenceWrapperTests.test("Dispatch/filter") {
  expectWrapperDispatch(
    direct.filter { _ in true },
    indirect.filter { _ in true }, dispatchLog.filter)
}

sequenceWrapperTests.test("Dispatch/_customContainsEquatableElement") {
  expectWrapperDispatch(
    direct._customContainsEquatableElement(OpaqueValue(1)),
    indirect._customContainsEquatableElement(OpaqueValue(1)),
    dispatchLog._customContainsEquatableElement)
}

sequenceWrapperTests.test("Dispatch/_preprocessingPass") {
  expectWrapperDispatch(
    direct._preprocessingPass { 1 },
    indirect._preprocessingPass { 1 },
    dispatchLog._preprocessingPass)
}

sequenceWrapperTests.test("Dispatch/_copyToContiguousArray") {
  expectWrapperDispatch(
    direct._copyToContiguousArray(), indirect._copyToContiguousArray(),
    dispatchLog._copyToContiguousArray)
}

runAllTests()
