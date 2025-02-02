//===--- Backtrace+Codable.swift ------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines the Codable conformance for Backtrace.
//
//===----------------------------------------------------------------------===//

import Swift

func stringFrom(sequence: some Sequence<UTF8.CodeUnit>) -> String? {
  if #available(macOS 15.0, *) {
    return String(validating: sequence, as: UTF8.self)
  } else {
    let bytes = Array(sequence)
    return String(decoding: bytes, as: UTF8.self)
  }
}

@available(macOS 15.0, *)
extension Backtrace: Codable {

  enum CodingKeys: CodingKey {
    case architecture
    case backtrace
    case imageMap
  }

  public init(from decoder: any Decoder) throws {
    let values = try decoder.container(keyedBy: CodingKeys.self)
    self.architecture = try values.decode(String.self, forKey: .architecture)

    let backtraceB64 = try values.decode(String.self, forKey: .backtrace)
    self.representation = Array(Base64Decoder(source: backtraceB64.utf8))

    if let imageMapB64 = try values.decodeIfPresent(String.self,
                                                    forKey: .imageMap) {
      self.images = ImageMap(compactImageMapData:
                               Base64Decoder(source: imageMapB64.utf8))
    } else {
      self.images = nil
    }
  }

  public func encode(to encoder: any Encoder) throws {
    var values = encoder.container(keyedBy: CodingKeys.self)
    try values.encode(architecture, forKey: .architecture)

    let backtraceB64 = stringFrom(sequence:
                                    Base64Encoder(source: self.representation))
    try values.encode(backtraceB64, forKey: .backtrace)

    if let imageMap = self.images {
      let encoder = CompactImageMapFormat.Encoder(imageMap)
      let imageMapB64 = stringFrom(sequence:
                                     Base64Encoder(source: encoder))
      try values.encode(imageMapB64, forKey: .imageMap)
    }
  }

}
