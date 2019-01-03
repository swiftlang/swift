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
  /// Represents a file of Swift source code. A SwiftSourceFile may point to a
  /// file on disk or it may directly contain the code to be parsed.
  public enum SourceFile {
    /// Creates a SwiftSourceFile instance representing a file on disk.
    case path(String)

    /// Creates a SwiftSourceFile instance directly containing the code to be
    /// parsed.
    case source(String)

    /// The filename to be used for EditorOpen and EditorClose commands.
    var name: String {
      switch self {
      case .path(let name):
        return name
      case .source:
        return "untitled.swift"
      }
    }
  }

  /// The format to serialize the syntax tree in.
  public enum SyntaxSerializationFormat {
    /// Return the syntax tree as JSON. The JSON serialization is more
    /// human-readable but less efficient to deserialize.
    case json

    /// Return the syntax tree as ByteTree. The ByteTree serialization is fast
    /// and compact but difficult to inspect.
    case byteTree

    /// Returns the corresponding kind_SyntaxTreeSerialization* UID.
    var kind: SourceKitdUID {
      switch self {
      case .json:
        return .kind_SyntaxTreeSerializationJSON
      case .byteTree:
        return .kind_SyntaxTreeSerializationByteTree
      }
    }

    /// Fetches the key_SerializedSyntaxTree entry from the response dictionary,
    /// converting it if necessary.
    func value(from dictionary: SourceKitdResponse.Dictionary) -> Data {
      let key = SourceKitdUID.key_SerializedSyntaxTree
      switch self {
      case .json:
        let string = dictionary.getString(key)
        return string.data(using: .utf8)!
      case .byteTree:
        return dictionary.getData(key)
      }
    }
  }

  /// Parses the SourceFile in question using the provided serialization format,
  /// but does not extract the result from the dictionary.
  fileprivate static func parseRaw(
    _ content: SourceFile,
    into serialization: SyntaxSerializationFormat
  ) throws -> SourceKitdResponse.Dictionary {
    let Service = SourceKitdService()
    let Request = SourceKitdRequest(uid: .request_EditorOpen)

    switch content {
    case .path(let path):
      Request.addParameter(.key_SourceFile, value: path)

    case .source(let content):
      Request.addParameter(.key_SourceText, value: content)
    }

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
    return Resp.value
  }

  /// Synchronously parses Swift source code into a syntax tree serialized in
  /// the indicated format.
  ///
  /// - Parameter content: The source code to parse.
  /// - Parameter serialization: The serialization format to use for the syntax
  ///   tree.
  /// - Returns: The syntax tree in the indicated serialization format.
  /// - Throws: If an error occurs when opening or closing the file.
  public static func parse(
    _ content: SourceFile,
    into serialization: SyntaxSerializationFormat
  ) throws -> Data {
    return serialization.value(from: try parseRaw(content, into: serialization))
  }
}

extension SwiftLang {
  /// Parses the Swift file at the provided URL into a `Syntax` tree in Json
  /// serialization format by querying SourceKitd service. This function isn't
  /// thread safe.
  /// - Parameter url: The URL you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  @available(swift, deprecated: 5.0, renamed: "parse(_:into:)")
  public static func parse(path: String) throws -> String {
    return try parseRaw(.path(path), into: .json)
      .getString(.key_SerializedSyntaxTree)
  }

  /// Parses a given source buffer into a `Syntax` tree in Json serialization
  /// format by querying SourceKitd service. This function isn't thread safe.
  /// - Parameter source: The source buffer you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  @available(swift, deprecated: 5.0, renamed: "parse(_:into:)")
  public static func parse(source: String) throws -> String {
    return try parseRaw(.source(source), into: .json)
      .getString(.key_SerializedSyntaxTree)
  }
}
