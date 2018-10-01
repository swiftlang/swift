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

  fileprivate static func parse(content: String, name: String, isURL: Bool) throws -> String {
    let Service = SourceKitdService()
    let Request = SourceKitdRequest(uid: .request_EditorOpen)
    if isURL {
      Request.addParameter(.key_SourceFile, value: content)
    } else {
      Request.addParameter(.key_SourceText, value: content)
    }
    Request.addParameter(.key_Name, value: name)
    Request.addParameter(.key_SyntaxTreeTransferMode,
                         value: .kind_SyntaxTreeFull)
    Request.addParameter(.key_EnableSyntaxMap, value: 0)
    Request.addParameter(.key_EnableStructure, value: 0)
    Request.addParameter(.key_SyntacticOnly, value: 1)

    // FIXME: SourceKitd error handling.
    let Resp = Service.sendSyn(request: Request)
    if Resp.isError {
      throw SourceKitdError.EditorOpenError(message: Resp.description)
    }

    let CloseReq = SourceKitdRequest(uid: .request_EditorClose)
    CloseReq.addParameter(.key_Name, value: name)
    let CloseResp = Service.sendSyn(request: CloseReq)
    if CloseResp.isError {
      throw SourceKitdError.EditorCloseError(message: CloseResp.description)
    }
    return Resp.value.getString(.key_SerializedSyntaxTree)
  }

  /// Parses the Swift file at the provided URL into a `Syntax` tree in Json
  /// serialization format by querying SourceKitd service. This function isn't
  /// thread safe.
  /// - Parameter url: The URL you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  public static func parse(path: String) throws -> String {
    return try parse(content: path, name: path, isURL: true)
  }

  /// Parses a given source buffer into a `Syntax` tree in Json serialization
  /// format by querying SourceKitd service. This function isn't thread safe.
  /// - Parameter source: The source buffer you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  public static func parse(source: String) throws -> String {
    return try parse(content: source, name: "foo", isURL: false)
  }
}
