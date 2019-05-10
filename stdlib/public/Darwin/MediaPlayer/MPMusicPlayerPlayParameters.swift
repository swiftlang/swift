//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import MediaPlayer
import Foundation

@available(iOS 11.0, *)
extension MPMusicPlayerPlayParameters: Codable {
    public required convenience init(from decoder: Decoder) throws {
        var playParametersDictionary: [String: Any] = [:]
        let values = try decoder.container(keyedBy: CodingKeys.self)

        if let id = try values.decodeIfPresent(String.self, forKey: .id) {
            playParametersDictionary[CodingKeys.id.rawValue] = id
        }
        else if let id = try values.decodeIfPresent(Int64.self, forKey: .id) {
            playParametersDictionary[CodingKeys.id.rawValue] = NSNumber(value: id)
        }

        if let kind = try values.decodeIfPresent(String.self, forKey: .kind) {
            playParametersDictionary[CodingKeys.kind.rawValue] = kind
        }

        if let isLibrary = try values.decodeIfPresent(Bool.self, forKey: .isLibrary) {
            playParametersDictionary[CodingKeys.isLibrary.rawValue] = NSNumber(value: isLibrary)
        }

        guard MPMusicPlayerPlayParameters(dictionary: playParametersDictionary) != nil else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath,
                                                                    debugDescription: "Attempted to decode MPMusicPlayerPlayParameters from invalid playParams dictionary."))
        }
        self.init(dictionary: playParametersDictionary)!
    }

    public func encode(to encoder: Encoder) throws {
        let playParametersDictionary = self.dictionary
        var values = encoder.container(keyedBy: CodingKeys.self)

        if let id = playParametersDictionary[CodingKeys.id.rawValue] as? String {
            try values.encode(id, forKey: .id)
        }
        else if let id = playParametersDictionary[CodingKeys.id.rawValue] as? Int64 {
            try values.encode(id, forKey: .id)
        }

        if let kind = playParametersDictionary[CodingKeys.kind.rawValue] as? String {
            try values.encode(kind, forKey: .kind)
        }

        if let isLibrary = playParametersDictionary[CodingKeys.isLibrary.rawValue] as? Bool {
            try values.encode(isLibrary, forKey: .isLibrary)
        }
    }

    internal enum CodingKeys: String, CodingKey {
        case id
        case kind
        case isLibrary
    }
}
