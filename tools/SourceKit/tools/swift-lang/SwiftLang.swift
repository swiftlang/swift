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

public class SwiftLang {

  /// Parses the Swift file at the provided URL into a `Syntax` tree in Json
  /// serialization format by querying SourceKitd service.
  /// - Parameter url: The URL you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  public static func parse(_ url: URL) throws -> String {
    let Service = SourceKitdService()
    let Request = SourceKitdRequest(uid: .request_EditorOpen)
    let Path = url.path
    Request.addParameter(.key_SourceFile, value: Path)
    Request.addParameter(.key_Name, value: Path)
    Request.addParameter(.key_EnableSyntaxTree, value: 1)

    // FIXME: SourceKitd error handling.
    let Resp = Service.sendSyn(request: Request)
    return Resp.value.getString(.key_SerializedSyntaxTree)
  }
}