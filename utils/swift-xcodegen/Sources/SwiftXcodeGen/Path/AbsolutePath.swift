//===--- AbsolutePath.swift -----------------------------------------------===//
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

import Foundation
import System

public struct AbsolutePath: PathProtocol, Sendable {
  public let storage: FilePath
  public init(_ storage: FilePath) {
    precondition(
      storage.isAbsolute, "'Expected \(storage)' to be an absolute path"
    )
    self.storage = storage.lexicallyNormalized()
  }
  public var asAnyPath: AnyPath {
    .absolute(self)
  }
}

public extension AbsolutePath {
  var isDirectory: Bool {
    var isDir: ObjCBool = false
    guard FileManager.default.fileExists(atPath: rawPath, isDirectory: &isDir)
    else {
      return false
    }
    return isDir.boolValue
  }

  func getDirContents() throws -> [RelativePath] {
    try FileManager.default.contentsOfDirectory(atPath: rawPath).map { .init($0) }
  }

  var exists: Bool {
    FileManager.default.fileExists(atPath: rawPath)
  }

  var isExecutable: Bool {
    FileManager.default.isExecutableFile(atPath: rawPath)
  }

  var isSymlink: Bool {
    (try? FileManager.default.destinationOfSymbolicLink(atPath: rawPath)) != nil
  }

  var realPath: Self {
    guard let resolved = realpath(rawPath, nil) else { return self }
    defer {
      free(resolved)
    }
    return Self(String(cString: resolved))
  }

  func makeDir(withIntermediateDirectories: Bool = true) throws {
    try FileManager.default.createDirectory(
      atPath: rawPath, withIntermediateDirectories: withIntermediateDirectories)
  }

  func remove() {
    try? FileManager.default.removeItem(atPath: rawPath)
  }

  func symlink(to dest: AbsolutePath) throws {
    try parentDir?.makeDir()
    if isSymlink {
      remove()
    }
    try FileManager.default.createSymbolicLink(
      atPath: rawPath, withDestinationPath: dest.rawPath
    )
  }

  func read() throws -> Data {
    try Data(contentsOf: URL(fileURLWithPath: rawPath))
  }

  func write(_ data: Data, as encoding: String.Encoding = .utf8) throws {
    try parentDir?.makeDir()
    FileManager.default.createFile(atPath: rawPath, contents: data)
  }

  func write(_ contents: String, as encoding: String.Encoding = .utf8) throws {
    try write(contents.data(using: encoding)!)
  }
}

extension AbsolutePath: ExpressibleByStringLiteral, ExpressibleByStringInterpolation {
  public init(stringLiteral value: StringLiteralType) {
    self.init(value)
  }
}

extension AbsolutePath: Decodable {
  public init(from decoder: Decoder) throws {
    let storage = FilePath(
      try decoder.singleValueContainer().decode(String.self)
    )
    guard storage.isAbsolute else {
      struct NotAbsoluteError: Error, Sendable {
        let path: FilePath
      }
      throw NotAbsoluteError(path: storage)
    }
    self.init(storage)
  }
}
