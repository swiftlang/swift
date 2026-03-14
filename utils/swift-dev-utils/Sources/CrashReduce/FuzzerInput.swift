//===--- FuzzerInput.swift ------------------------------------------------===//
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

/// The input information for an input crasher produced by the fuzzer.
struct FuzzerInput {
  var path: AbsolutePath
  var header: Header
  var buffers: [Buffer] = []

  init(from path: AbsolutePath) throws {
    self.path = path
    (self.header, self.buffers) = try Self.parse(Code(from: path))
  }
}

extension FuzzerInput {
  private static func takeHeader(_ text: String) -> (Header, Code) {
    guard text.hasPrefix("// {"), let newline = text.firstIndex(of: "\n"),
          let header = Header(from: text.dropFirst(3)[..<newline]) else {
      return (Header(), Code(text))
    }
    return (header, Code(text[text.index(after: newline)...]))
  }

  private static func parse(_ input: Code) throws -> (Header, [Buffer]) {
    let (header, code) = takeHeader(input.text)
    return (header, Buffer.makeDefault(try code.split(header.splits)))
  }
}

extension FuzzerInput {
  struct Header: Codable {
    enum CodingKeys: String, CodingKey {
      case _splits = "splits"
      case _sourceKitRequests = "sourceKitRequests"
      case _frontendArgs = "frontendArgs"
    }
    private var _splits: [Int]?
    var splits: [Int] { _splits ?? [] }

    private var _sourceKitRequests: [SourceKitRequest]?
    var sourceKitRequests: [SourceKitRequest] { _sourceKitRequests ?? [] }

    var _frontendArgs: [String]?
    var frontendArgs: [Command.Argument]? {
      guard let _frontendArgs else { return nil }
      return _frontendArgs.map { .value($0) }
    }
  }

  struct SourceKitRequest: Codable {
    enum Kind: String, Codable {
      case complete, cursorInfo
    }
    enum CodingKeys: String, CodingKey {
      case kind, offset
      case _fileIdx = "fileIdx"
    }
    var kind: Kind
    var offset: Int
    var _fileIdx: Int?
    var fileIdx: Int { _fileIdx ?? 0 }
  }
}

extension FuzzerInput.Header {
  init?(from str: some StringProtocol) {
    guard let header = try? JSONDecoder().decode(Self.self, from: Data(str.utf8)) else {
      return nil
    }
    self = header
  }
}

extension FuzzerInput: CustomStringConvertible {
  var description: String {
    "\(path.fileName)"
  }
}
