//===--- github_repository.swift ------------------------------------------===//
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

import ArgumentParser

enum Platform: String, EnumerableFlag {
  case osx
  case ubuntu1404
  case ubuntu1604
  case ubuntu1804

  var fileType: String {
    switch self {
    case .osx:
      return "pkg"
    case .ubuntu1404,
         .ubuntu1604,
         .ubuntu1804:
      return "tar.gz"
    }
  }

  var toolchainType: String {
    switch self {
    case .osx:
      return "xcode"
    case .ubuntu1404,
         .ubuntu1604,
         .ubuntu1804:
      return self.rawValue
    }
  }
}

enum Branch: String, EnumerableFlag {
  case development
  case release_5_0 = "5.0"
  case release_6_0 = "6.0"
  case release_6_2 = "6.2"

  var tagPrefix: String {
    switch self {
    case .development:
      "swift-\(rawValue.uppercased())"
    default:
      "swift-\(rawValue.uppercased())-DEVELOPMENT"
    }
  }

  var urlBranchName: String {
    switch self {
    case .development:
      return self.rawValue
    default:
      return "swift-\(self.rawValue)-branch"
    }
  }
}
