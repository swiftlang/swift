//===--- Repo.swift -------------------------------------------------------===//
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

// TODO: This really ought to be defined in swift-xcodegen
public enum Repo: CaseIterable, Sendable {
  case swift
  case swiftRuntimes
  case lldb
  case llvm
  case cmark

  public var relativePath: RelativePath {
    switch self {
    case .swift: "swift"
    case .swiftRuntimes: "swift/Runtimes"
    case .cmark: "cmark"
    case .lldb:  "llvm-project/lldb"
    case .llvm:  "llvm-project"
    }
  }

  public var buildDirPrefix: String? {
    switch self {
    case .swift: "swift"
    case .swiftRuntimes: nil
    case .cmark: "cmark"
    case .lldb:  "lldb"
    case .llvm:  "llvm"
    }
  }
}
