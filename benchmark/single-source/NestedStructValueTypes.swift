//===--- NestedStructValueTypes.swift ------------------------------------===//
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

import TestsUtils

// This is a benchmark meant for use measuring the size of value witnesses of nested
// structs.

public let benchmarks = BenchmarkInfo(
  name: "NestedValue.Struct.CodeSize",
  runFunction: run_NestedStructValueTypes,
  tags: [.skip])

public final class Reference {
  public init() {}
}

public struct PODLeaf {
  public var a: UInt64
  public var b: UInt64
  public var c: UInt64
  public var d: UInt64
  public var e: UInt64
  public var f: UInt64
}

public struct ReferenceLeaf {
  public var a: Reference
  public var b: Reference
}

public struct Middle {
  public var firstPOD: PODLeaf
  public var secondPOD: PODLeaf
  public var references: ReferenceLeaf
}

public struct Outer {
  public var first: Middle
  public var second: Middle
}

public func copy(_ value: Outer) -> Outer {
  value
}

public func assign(_ destination: inout Outer, _ source: Outer) {
  destination = source
}

public func run_NestedStructValueTypes(_ n: Int) {}
