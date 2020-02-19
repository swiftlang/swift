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

  public func hash(into hasher: inout Hasher) {
    hasher.combine(uid)
  }
}
