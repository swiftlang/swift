//===--- FunctionInfo.swift - PDB support for Swift -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines a structure used to hold information about a function.
//
//===----------------------------------------------------------------------===//

import Swift

public struct SectionCharacteristics: OptionSet, Sendable {
  public typealias RawValue = UInt32

  public var rawValue: UInt32

  public init(rawValue: UInt32) {
    self.rawValue = rawValue
  }

  public static let noPad
    = SectionCharacteristics(rawValue: 0x00000008)
  public static let code
    = SectionCharacteristics(rawValue: 0x00000020)
  public static let initializedData
    = SectionCharacteristics(rawValue: 0x00000040)
  public static let uninitializedData
    = SectionCharacteristics(rawValue: 0x00000080)
  public static let noDeferSpeculativeExecution
    = SectionCharacteristics(rawValue: 0x00004000)
  public static let gpRelative
    = SectionCharacteristics(rawValue: 0x00008000)
  public static let hasExtendedRelocations
    = SectionCharacteristics(rawValue: 0x01000000)
  public static let discardable
    = SectionCharacteristics(rawValue: 0x02000000)
  public static let notCached
    = SectionCharacteristics(rawValue: 0x04000000)
  public static let notPaged
    = SectionCharacteristics(rawValue: 0x08000000)
  public static let shared
    = SectionCharacteristics(rawValue: 0x10000000)
  public static let executable
    = SectionCharacteristics(rawValue: 0x20000000)
  public static let readable
    = SectionCharacteristics(rawValue: 0x40000000)
  public static let writable
    = SectionCharacteristics(rawValue: 0x80000000)
}

public struct SectionInfo {
  var name: String
  var virtualAddress: UInt32
  var size: UInt32
  var characteristics: SectionCharacteristics
}
