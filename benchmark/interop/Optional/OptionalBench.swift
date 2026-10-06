//===--- OptionalBench.swift -----------------------------------------------===//
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

public func integer(_ value: Int64) -> Int64? { value }
public func string() -> String? { String(repeating: "abcd", count: 256) }
public func array() -> [Int64]? { [11, 22, 33] }

@frozen public struct Large {
  public var a, b, c, d: Int64
  public init(_ value: Int64) {
    a = value
    b = value
    c = value
    d = value
  }
}

@_alignment(16)
@frozen public struct Aligned {
  public var value: UInt8
  public init(_ value: UInt8) { self.value = value }
}

public func large() -> Large? { Large(42) }
public func aligned() -> Aligned? { Aligned(7) }
