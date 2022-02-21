//===--- SourceLoc.swift - SourceLoc bridiging utilities ------------------===//
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

public struct SourceLoc {
  public var pointer: UnsafePointer<UInt8>

  public init?(pointer: UnsafePointer<UInt8>?) {
    guard let pointer = pointer else {
      return nil
    }
    self.pointer = pointer
  }

  public init?(pointer: UnsafePointer<CChar>?) {
    guard let pointer = pointer else {
      return nil
    }
    self.pointer = UnsafeRawPointer(pointer).assumingMemoryBound(to: UInt8.self)
  }

  public init?(bridged: BridgedSourceLoc) {
    guard let pointer = bridged.pointer else {
      return nil
    }
    self.init(pointer: pointer)
  }

  public var bridged: BridgedSourceLoc {
    .init(pointer: pointer)
  }
}

extension Optional where Wrapped == SourceLoc {
  public var bridged: BridgedSourceLoc {
    self?.bridged ?? .init(pointer: nil)
  }
}

public struct CharSourceRange {
  public var start: SourceLoc
  public var byteLength: Int

  public init(start: SourceLoc, byteLength: Int) {
    self.start = start
    self.byteLength = byteLength
  }

  public init?(bridged: BridgedCharSourceRange) {
    guard let start = SourceLoc(bridged: bridged.start) else {
      return nil
    }
    self.init(start: start, byteLength: bridged.byteLength)
  }

  public var bridged: BridgedCharSourceRange {
    .init(start: start.bridged, byteLength: byteLength)
  }
}

extension Optional where Wrapped == CharSourceRange {
  public var bridged: BridgedCharSourceRange {
    self?.bridged ?? .init(start: .init(pointer: nil), byteLength: 0)
  }
}
