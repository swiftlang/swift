//===------------------------ SourceKitdUID.swift -------------------------===//
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
// This file provides SourceKitd UIDs.
//===----------------------------------------------------------------------===//

import sourcekitd

public struct SourceKitdUID: Equatable, Hashable, CustomStringConvertible {
  public let uid: sourcekitd_uid_t

  init(uid: sourcekitd_uid_t) {
    self.uid = uid
  }

  public init(string: String) {
    self.uid = sourcekitd_uid_get_from_cstr(string)
  }

  public var description: String {
    return String(cString: sourcekitd_uid_get_string_ptr(uid))
  }

  public var asString: String {
    return String(cString: sourcekitd_uid_get_string_ptr(uid))
  }

  public var hashValue: Int {
    return uid.hashValue
  }
}

extension SourceKitdUID {
  public static let key_request = SourceKitdUID(string: "key.request")
  public static let key_compilerargs = SourceKitdUID(string: "key.compilerargs")
  public static let key_notification = SourceKitdUID(string: "key.notification")
  public static let key_name = SourceKitdUID(string: "key.name")
  public static let key_enable_syntax_tree = SourceKitdUID(string: "key.enablesyntaxtree")
  public static let key_serialized_syntax_tree = SourceKitdUID(string: "key.serialized_syntax_tree")
  public static let key_sourcefile = SourceKitdUID(string: "key.sourcefile")
  public static let compilerCrashedNotification = SourceKitdUID(string: "notification.toolchain-compiler-crashed")
  public static let source_request_editor_open = SourceKitdUID(string: "source.request.editor.open")
  public static let source_notification_editor_documentupdate = SourceKitdUID(string: "source.notification.editor.documentupdate")
}
