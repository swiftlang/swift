//===--- AnyPath.swift ----------------------------------------------------===//
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

import ArgumentParser
import System

public enum AnyPath: PathProtocol, Sendable {
  case relative(RelativePath)
  case absolute(AbsolutePath)

  public init<P: PathProtocol>(_ path: P) {
    self = path.asAnyPath
  }

  public init(_ storage: FilePath) {
    if storage.isAbsolute {
      self = .absolute(.init(storage))
    } else {
      self = .relative(.init(storage))
    }
  }

  public var storage: FilePath {
    switch self {
    case .relative(let r):
      r.storage
    case .absolute(let a):
      a.storage
    }
  }

  public var asAnyPath: AnyPath {
    self
  }
}

extension AnyPath {
  public var absoluteInWorkingDir: AbsolutePath {
    switch self {
    case .relative(let r):
      r.absoluteInWorkingDir
    case .absolute(let a):
      a
    }
  }

  public func absolute(in base: AbsolutePath) -> AbsolutePath {
    switch self {
    case .relative(let r):
      r.absolute(in: base)
    case .absolute(let a):
      a
    }
  }
}

extension AnyPath: Decodable {
  public init(from decoder: Decoder) throws {
    self.init(try decoder.singleValueContainer().decode(String.self))
  }
}

extension AnyPath: ExpressibleByArgument {
  public init(argument rawPath: String) {
    self.init(rawPath)
  }
}
