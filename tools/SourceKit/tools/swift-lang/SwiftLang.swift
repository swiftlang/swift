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

public class SwiftLang {
  private static let syntaxTreeKey = SourceKitdUID.key_SerializedSyntaxTree

  fileprivate struct SourceFile {
    let name: String
    let contentKey: SourceKitdUID
    let contentValue: String

    static func path(_ path: String) -> SourceFile {
      return SourceFile(
        name: path,
        contentKey: .key_SourceFile,
        contentValue: path
      )
    }

    static func source(_ source: String) -> SourceFile {
      return SourceFile(
        name: "foo",
        contentKey: .key_SourceText,
        contentValue: source
      )
    }
  }

  /// The format to serialize the syntax tree in.
  public struct SyntaxTreeFormat<Tree> {
    /// Serialize the syntax tree into a JSON string. For backwards
    /// compatibility only.
    fileprivate static var jsonString: SyntaxTreeFormat<String> {
      return .init(kind: .kind_SyntaxTreeSerializationJSON) { dict in
        dict.getString(syntaxTreeKey)
      }
    }

    /// Return the syntax tree as JSON. The JSON serialization is more
    /// human-readable but less efficient to deserialize.
    public static var json: SyntaxTreeFormat<Data> {
      return jsonString.withTreeMapped { $0.data(using: .utf8)! }
    }

    /// Return the syntax tree as ByteTree. The ByteTree serialization is fast
    /// and compact but difficult to inspect.
    public static var byteTree: SyntaxTreeFormat<Data> {
      return .init(kind: .kind_SyntaxTreeSerializationByteTree) { dict in
        dict.getData(syntaxTreeKey)
      }
    }

    /// Value for EditorOpen's key_SyntaxTreeSerializationFormat key.
    let kind: SourceKitdUID

    /// Extracts the syntax tree from the response dictionary and converts it
    /// into a value of type Tree.
    let makeTree: (SourceKitdResponse.Dictionary) throws -> Tree

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
    ) -> SyntaxTreeFormat<NewTree> {
      return .init(kind: kind) { [makeTree] dict in
        try transform(makeTree(dict))
      }
    }
  }

  /// Parses the SourceFile in question using the provided serialization format.
  fileprivate static func parse<Tree>(
    _ content: SourceFile,
    into serialization: SyntaxTreeFormat<Tree>
  ) throws -> Tree {
    let Service = SourceKitdService()
    let Request = SourceKitdRequest(uid: .request_EditorOpen)

    Request.addParameter(content.contentKey, value: content.contentValue)
    Request.addParameter(.key_Name, value: content.name)

    Request.addParameter(.key_SyntaxTreeTransferMode,
                         value: .kind_SyntaxTreeFull)
    Request.addParameter(.key_SyntaxTreeSerializationFormat,
                         value: serialization.kind)
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
    return try serialization.makeTree(Resp.value)
  }

  /// Synchronously parses Swift source code into a syntax tree serialized in
  /// the indicated format.
  ///
  /// - Parameter url: A file URL pointing to a Swift source file.
  /// - Parameter serialization: The serialization format to use for the syntax
  ///   tree.
  /// - Returns: The syntax tree in the indicated serialization format.
  /// - Throws: If SourceKit responds to the request with an error. This is
  ///           usually a communication or configuration error, not a
  ///           syntax error in the code being parsed.
  /// - Precondition: `url` must be a file URL.
  public static func parse<Tree>(
    contentsOf url: URL, into serialization: SyntaxTreeFormat<Tree>
  ) throws -> Tree {
    precondition(url.isFileURL, "Can only parse files at file URLs")
    return try parse(.path(url.path), into: serialization)
  }
}

extension SwiftLang {
  /// Parses the Swift file at the provided URL into a `Syntax` tree in Json
  /// serialization format by querying SourceKitd service. This function isn't
  /// thread safe.
  /// - Parameter path: The URL you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  @available(swift, deprecated: 5.0, renamed: "parse(_:into:)")
  public static func parse(path: String) throws -> String {
    return try parse(.path(path), into: .jsonString)
  }

  /// Parses a given source buffer into a `Syntax` tree in Json serialization
  /// format by querying SourceKitd service. This function isn't thread safe.
  /// - Parameter source: The source buffer you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  @available(swift, deprecated: 5.0, message: "Use parse(_:into:) with a file instead")
  public static func parse(source: String) throws -> String {
    return try parse(.source(source), into: .jsonString)
  }
}
