//===---------------- SourcePresence.swift - Source Presence --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

/// An indicator of whether a Syntax node was found or written in the source.
///
/// A `missing` node does not mean, necessarily, that the source item is
/// considered "implicit", but rather that it was not found in the source.
public enum SourcePresence: String, Codable {
  /// The syntax was authored by a human and found, or was generated.
  case present = "Present"

  /// The syntax was expected or optional, but not found in the source.
  case missing = "Missing"
}

extension SourcePresence: ByteTreeScalarDecodable {
  static func read(from pointer: UnsafeRawPointer, size: Int,
                   userInfo: UnsafePointer<[ByteTreeUserInfoKey: Any]>
  ) -> SourcePresence {
    let rawValue = pointer.bindMemory(to: UInt8.self, capacity: 1).pointee
    switch rawValue {
    case 0: return .missing
    case 1: return .present
    default:
      fatalError("Unknown source presence \(rawValue)")
    }
  }
}
