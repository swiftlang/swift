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
  public enum MetadataKind : Int {
    case `class` = 0
    case `struct` = 1
    case `enum` = 2
    case optional = 3
    case opaque = 8
    case tuple = 9
    case function = 10
    case existential = 12
    case metatype = 13
    case objCClassWrapper = 14
    case existentialMetatype = 15
    case foreignClass = 16
    case heapLocalVariable = 64
    case heapGenericLocalVariable = 65
    case errorObject = 128
  }

  @_silgen_name("swift_StdlibUnittest_getMetadataKindOf")
  static func _metadataKindImpl<T>(of value: T) -> UInt32

  public static func metadataKind<T>(of value: T) -> MetadataKind {
    return MetadataKind(rawValue: Int(_metadataKindImpl(of: value)))!
  }
}

