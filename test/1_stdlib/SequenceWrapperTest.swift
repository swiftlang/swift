//===--- SequenceWrapperTest.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
import StdlibUnittest

struct BasicSequenceWrapper<
  Base_: SequenceType
> : _SequenceWrapperType, SequenceType {
  var _base: Base_
}

var sequenceWrapperTests = TestSuite("SequenceWrapper")

let base = LoggingSequence([OpaqueValue(1)])
let direct = BasicSequenceWrapper(_base: base)
let indirect = LoggingSequence(direct)
let dispatchLog = base.log

func expectWrapperDispatch<R1, R2>(
  @autoclosure directOperation: ()->R1,
  @autoclosure _ indirectOperation: ()->R2,
  _ counters: TypeIndexed<Int>,
  stackTrace: SourceLocStack? = nil,
  file: String = __FILE__, line: UWord = __LINE__,
  collectMoreInfo: (()->String)? = nil
) {
  counters.reset()
  _ = directOperation()
  expectEqual(
    [base.selfType: 1], counters, stackTrace: stackTrace,
    file: file, line: line, collectMoreInfo: collectMoreInfo
  )
  counters.reset()
  _ = indirectOperation()
  expectEqual(
    [base.selfType: 1, indirect.selfType: 1], counters, stackTrace: stackTrace,
    file: file, line: line, collectMoreInfo: collectMoreInfo
  )
}

sequenceWrapperTests.test("Dispatch/generate") {
  expectWrapperDispatch(
    direct.generate(), indirect.generate(), dispatchLog.generate)
}

sequenceWrapperTests.test("Dispatch/underestimateCount") {
  expectWrapperDispatch(
    direct.underestimateCount(), indirect.underestimateCount(),
    dispatchLog.underestimateCount)
}

sequenceWrapperTests.test("Dispatch/map") {
  expectWrapperDispatch(
    direct.map { $0 }, indirect.map { $0 }, dispatchLog.map)
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
    direct._preprocessingPass { _ in 1 },
    indirect._preprocessingPass { _ in 1 },
    dispatchLog._preprocessingPass)
}

sequenceWrapperTests.test("Dispatch/_copyToNativeArrayBuffer") {
  expectWrapperDispatch(
    direct._copyToNativeArrayBuffer(), indirect._copyToNativeArrayBuffer(),
    dispatchLog._copyToNativeArrayBuffer)
}

runAllTests()
