//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// namespace
public enum SwiftRuntime {
  @_silgen_name("getMetadataKindOf")
  private static func _metadataKind<T>(of value: T) -> UnsafePointer<CChar>

  public static func metadataKind<T>(of value: T) -> String {
    return String(validatingCString: _metadataKind(of: value))!
  }
}

