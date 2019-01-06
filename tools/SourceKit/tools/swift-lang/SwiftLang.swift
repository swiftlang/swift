//===--------------------- SwiftLang.swift -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file provides Swift language support by invoking SourceKit internally.
//===----------------------------------------------------------------------===//

import Foundation

enum SourceKitdError: Error, CustomStringConvertible {
  case EditorOpenError(message: String)
  case EditorCloseError(message: String)

  var description: String {
    switch self {
    case .EditorOpenError(let message):
      return "cannot open document: \(message)"
    case .EditorCloseError(let message):
      return "cannot close document: \(message)"
    }
  }
}

// MARK: Public APIs

public enum SwiftLang {
  /// Synchronously parses Swift source code into a syntax tree.
  ///
  /// - Parameter url: A file URL pointing to a Swift source file.
  /// - Parameter format: The format to use for the returned syntax tree.
  /// - Returns: The syntax tree in the indicated format.
  /// - Throws: If SourceKit responds to the request with an error. This is
  ///           usually a communication or configuration error, not a
  ///           syntax error in the code being parsed.
  /// - Precondition: `url` must be a file URL.
  public static func parse<Tree>(
    contentsOf url: URL, into format: SyntaxTreeFormat<Tree>
  ) throws -> Tree {
    precondition(url.isFileURL, "Can only parse files at file URLs")
    return try parse(SourceFile(path: url.path), into: format)
  }
}

extension SwiftLang.SyntaxTreeFormat {
  /// Return the syntax tree serialized in JSON format. JSON is easy to inspect
  /// and test with but very inefficient to deserialize. Use it for testing and
  /// debugging.
  public static var json: SwiftLang.SyntaxTreeFormat<Data> {
    return jsonString.withTreeMapped { $0.data(using: .utf8)! }
  }

  /// Return the syntax tree serialized as ByteTree. ByteTree is fast and
  /// compact but difficult to inspect or manipulate with textual tools.
  /// It's recommended in production.
  public static var byteTree: SwiftLang.SyntaxTreeFormat<Data> {
    return .init(kind: .kind_SyntaxTreeSerializationByteTree) { dict in
      dict.getData(syntaxTreeKey)
    }
  }

  /// Creates a new `SyntaxTreeFormat` instance which converts the tree of an
  /// existing format.
  ///
  /// You can use this method to add new `SyntaxTreeFormat`s; simply declare a
  /// new static constant in an extension of `SyntaxTreeFormat` which maps one
  /// of the existing formats.
  ///
  /// - Parameter transform: A function which converts `self`'s Tree type to
  ///             the result type's Tree type.
  /// - Returns: A new format which creates a tree in `self`'s format, then
  ///            applies `transform` to the tree to convert it.
  public func withTreeMapped<NewTree>(
    by transform: @escaping (Tree) throws -> NewTree
  ) -> SwiftLang.SyntaxTreeFormat<NewTree> {
    return .init(kind: kind) { [makeTree] dict in
      try transform(makeTree(dict))
    }
  }
}

// MARK: Deprecated APIs

extension SwiftLang {
  /// Parses the Swift file at the provided URL into a `Syntax` tree in Json
  /// serialization format by querying SourceKitd service. This function isn't
  /// thread safe.
  /// - Parameter path: The URL you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  @available(swift, deprecated: 5.0, renamed: "parse(_:into:)")
  public static func parse(path: String) throws -> String {
    return try parse(SourceFile(path: path), into: .jsonString)
  }

  /// Parses a given source buffer into a `Syntax` tree in Json serialization
  /// format by querying SourceKitd service. This function isn't thread safe.
  /// - Parameter source: The source buffer you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  @available(swift, deprecated: 5.0, message: "use parse(_:into:) with a file instead")
  public static func parse(source: String) throws -> String {
    let content = SourceFile(
      name: "foo", contentKey: .key_SourceText, contentValue: source
    )
    return try parse(content, into: .jsonString)
  }
}

// MARK: Internals

extension SwiftLang {
  /// The format to serialize the syntax tree in.
  public struct SyntaxTreeFormat<Tree> {
    /// Value for EditorOpen's key_SyntaxTreeSerializationFormat key.
    fileprivate let kind: SourceKitdUID

    /// Extracts the syntax tree from the response dictionary and converts it
    /// into a value of type Tree.
    fileprivate let makeTree: (SourceKitdResponse.Dictionary) throws -> Tree

    /// Serialize the syntax tree into a JSON string. For backwards
    /// compatibility only.
    fileprivate static var jsonString: SyntaxTreeFormat<String> {
      return .init(kind: .kind_SyntaxTreeSerializationJSON) { dict in
        dict.getString(syntaxTreeKey)
      }
    }
  }
}

extension SwiftLang {
  fileprivate struct SourceFile {
    let name: String
    let contentKey: SourceKitdUID
    let contentValue: String
  }

  /// Parses the SourceFile in question using the provided serialization format.
  fileprivate static func parse<Tree>(
    _ content: SourceFile, into format: SyntaxTreeFormat<Tree>
  ) throws -> Tree {
    let Service = SourceKitdService()
    let Request = SourceKitdRequest(uid: .request_EditorOpen)

    Request.addParameter(content.contentKey, value: content.contentValue)
    Request.addParameter(.key_Name, value: content.name)

    Request.addParameter(.key_SyntaxTreeTransferMode,
                         value: .kind_SyntaxTreeFull)
    Request.addParameter(.key_SyntaxTreeSerializationFormat,
                         value: format.kind)
    Request.addParameter(.key_EnableSyntaxMap, value: 0)
    Request.addParameter(.key_EnableStructure, value: 0)
    Request.addParameter(.key_SyntacticOnly, value: 1)

    // FIXME: SourceKitd error handling.
    let Resp = Service.sendSyn(request: Request)
    if Resp.isError {
      throw SourceKitdError.EditorOpenError(message: Resp.description)
    }

    let CloseReq = SourceKitdRequest(uid: .request_EditorClose)
    CloseReq.addParameter(.key_Name, value: content.name)
    let CloseResp = Service.sendSyn(request: CloseReq)
    if CloseResp.isError {
      throw SourceKitdError.EditorCloseError(message: CloseResp.description)
    }
    return try format.makeTree(Resp.value)
  }
}

extension SwiftLang.SourceFile {
  init(path: String) {
    self.init(name: path, contentKey: .key_SourceFile, contentValue: path)
  }
}

private let syntaxTreeKey = SourceKitdUID.key_SerializedSyntaxTree
