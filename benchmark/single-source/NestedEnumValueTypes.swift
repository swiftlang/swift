//===--- NestedEnumValueTypes.swift --------------------------------------===//
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
// enums.

public let benchmarks = BenchmarkInfo(
  name: "NestedValue.Enum.CodeSize",
  runFunction: run_NestedEnumValueTypes,
  tags: [.skip])

public final class Reference {
  public init() {}
}

public struct PODPayload {
  public var a: UInt64
  public var b: UInt64
  public var c: UInt64
  public var d: UInt64
  public var e: UInt64
  public var f: UInt64
}

public struct ReferencePayload {
  public var a: Reference
  public var b: Reference
}

public struct MixedPayload {
  public var pod: PODPayload
  public var references: ReferencePayload
}

public enum EnumLeaf {
  case payload(MixedPayload)
  case empty
}

public enum EnumMiddle {
  case payload(EnumLeaf, EnumLeaf)
  case empty
}

public enum EnumOuter {
  case payload(EnumMiddle, EnumMiddle)
  case empty
}

public func copy(_ value: EnumOuter) -> EnumOuter {
  value
}

public func assign(_ destination: inout EnumOuter, _ source: EnumOuter) {
  destination = source
}

public func run_NestedEnumValueTypes(_ n: Int) {}
