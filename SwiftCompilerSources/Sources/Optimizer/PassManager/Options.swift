//===--- Options.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

struct Options {
  let _bridged: BridgedPassContext

  var enableStackProtection: Bool {
    _bridged.enableStackProtection()
  }

  var useAggressiveReg2MemForCodeSize : Bool {
    _bridged.useAggressiveReg2MemForCodeSize()
  }

  var enableMoveInoutStackProtection: Bool {
    _bridged.enableMoveInoutStackProtection()
  }

  func enableSimplification(for inst: Instruction) -> Bool {
    _bridged.enableSimplificationFor(inst.bridged)
  }

  func enableAddressDependencies() -> Bool {
    _bridged.enableAddressDependencies()
  }

  var noAllocations: Bool {
    _bridged.noAllocations()
  }

  var enableEmbeddedSwift: Bool {
    hasFeature(.Embedded)
  }

  var enableMergeableTraps: Bool {
    _bridged.enableMergeableTraps()
  }

  func hasFeature(_ feature: BridgedFeature) -> Bool {
    _bridged.hasFeature(feature)
  }

  // The values for the assert_configuration call are:
  // 0: Debug
  // 1: Release
  // 2: Fast / Unchecked
  enum AssertConfiguration {
    case debug
    case release
    case unchecked
    case unknown

    var integerValue: Int {
      switch self {
      case .debug:      return 0
      case .release:    return 1
      case .unchecked:  return 2
      case .unknown:    fatalError()
      }
    }
  }

  var assertConfiguration: AssertConfiguration {
    switch _bridged.getAssertConfiguration() {
      case .Debug:               return .debug
      case .Release:             return .release
      case .Unchecked:           return .unchecked
      default:                   return .unknown
    }
  }
}
