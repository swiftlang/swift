//===--- BuildConfiguration.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public enum BuildConfiguration: String {
  case release = "Release"
  case releaseWithDebugInfo = "RelWithDebInfo"
  case debug = "Debug"
}

extension BuildConfiguration {
  public var hasDebugInfo: Bool {
    switch self {
    case .release:
      false
    case .releaseWithDebugInfo, .debug:
      true
    }
  }
}
