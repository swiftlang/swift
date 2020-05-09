//===--- RuntimeUnittest.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// Make runtime unit tests available to the stdlib unit test harness.
//
//===----------------------------------------------------------------------===//

// namespace
public enum SwiftRuntimeUnitTest {
  @_silgen_name("testExclusivityNullPC")
  private static func _testExclusivityNullPC()

  public static func testExclusivityNullPC() {
    _testExclusivityNullPC()
  }

  @_silgen_name("testExclusivityPCOne")
  private static func _testExclusivityPCOne()

  public static func testExclusivityPCOne() {
    _testExclusivityPCOne()
  }

  @_silgen_name("testExclusivityBogusPC")
  private static func _testExclusivityBogusPC()

  public static func testExclusivityBogusPC() {
    _testExclusivityBogusPC()
  }

  @_silgen_name("testExclusivityNonNested")
  private static func _testExclusivityNonNestedPC()

  public static func testExclusivityNonNestedPC() {
    _testExclusivityNonNestedPC()
  }
}
