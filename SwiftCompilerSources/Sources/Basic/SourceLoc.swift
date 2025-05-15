//===--- SourceLoc.swift - SourceLoc bridging utilities ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import BasicBridging

/// Represents a location in source code.
/// It is basically a pointer into a buffer of the loaded source file (managed by `DiagnosticEngine`).
/// In contrast to just having a filename+line+column, this allows displaying the context around
/// the location when printing diagnostics.
public struct SourceLoc {
  public let bridged: BridgedSourceLoc

  public init?(bridged: BridgedSourceLoc) {
    guard bridged.isValid else {
      return nil
    }
    self.bridged = bridged
  }
}

extension SourceLoc {
  public func advanced(by n: Int) -> SourceLoc {
    SourceLoc(bridged: bridged.advanced(by: n))!
  }
}

extension Optional where Wrapped == SourceLoc {
  public var bridged: BridgedSourceLoc {
    self?.bridged ?? .init()
  }
}

public struct CharSourceRange {
  public let start: SourceLoc
  public let byteLength: UInt32

  public init(start: SourceLoc, byteLength: UInt32) {
    self.start = start
    self.byteLength = byteLength
  }

  public init?(bridgedStart: BridgedSourceLoc, byteLength: UInt32) {
    guard let start = SourceLoc(bridged: bridgedStart) else {
      return nil
    }
    self.init(start: start, byteLength: byteLength)
  }
}
