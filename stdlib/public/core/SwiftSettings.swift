//===--- SwiftSettings.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if $Macros

@available(SwiftStdlib 9999, *)
public struct SwiftSetting {
  public init() {
    fatalError("A SwiftSetting should never actually be constructed")
  }
}

@available(SwiftStdlib 9999, *)
@freestanding(declaration)
public macro SwiftSettings(_ settings: SwiftSetting...) =
  Builtin.SwiftSettingsMacro

#endif
