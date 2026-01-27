//===--- TemporaryFile.swift ----------------------------------------------===//
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

import Foundation

fileprivate struct FailedToCreateTemp: Error {}

private func getTemporaryPath(prefix: String) throws -> AbsolutePath {
  let suffix = UUID().uuidString
  let name = prefix.isEmpty ? suffix : "\(prefix)-\(suffix)"
  let path = AbsolutePath(NSTemporaryDirectory()).appending(name)
  guard !path.exists else {
    throw FailedToCreateTemp()
  }
  return path
}

private func makeTemporaryDirectory(prefix: String) throws -> AbsolutePath {
  let dir = try getTemporaryPath(prefix: prefix)
  try dir.makeDir()
  return dir
}

private func makeTemporaryFile(prefix: String) throws -> AbsolutePath {
  let file = try getTemporaryPath(prefix: prefix)
  try file.touch()
  return file
}

public func withTemporaryDirectory<R>(
  prefix: String = "", _ body: (AbsolutePath) async throws -> R
) async throws -> R {
  let dir = try makeTemporaryDirectory(prefix: prefix)
  defer {
    dir.remove()
  }
  return try await body(dir)
}

public func withTemporaryDirectory<R>(
  prefix: String = "", _ body: (AbsolutePath) throws -> R
) throws -> R {
  let dir = try makeTemporaryDirectory(prefix: prefix)
  defer {
    dir.remove()
  }
  return try body(dir)
}

public func withTemporaryFile<R>(
  prefix: String = "", _ body: (AbsolutePath) async throws -> R
) async throws -> R {
  let file = try makeTemporaryFile(prefix: prefix)
  defer {
    file.remove()
  }
  return try await body(file)
}

public func withTemporaryFile<R>(
  prefix: String = "", _ body: (AbsolutePath) throws -> R
) throws -> R {
  let file = try makeTemporaryFile(prefix: prefix)
  defer {
    file.remove()
  }
  return try body(file)
}
