//===--- DeclRef.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SILBridging

public struct DeclRef: CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedDeclRef

  public var location: Location { Location(bridged: bridged.getLocation()) }

  public var description: String { String(taking: bridged.getDebugDescription()) }
}
