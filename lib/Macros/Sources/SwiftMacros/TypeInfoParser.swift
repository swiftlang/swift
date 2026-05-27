//===----------------------------------------------------------------------===//
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
// Utilities to handle type information passed as arguments to macros         //
// conformances.                                                              //
//===----------------------------------------------------------------------===//

public struct NominalTypeInfo {
  var kind: NominalTypeKind
  var isUnsafe: Bool
}

public enum NominalTypeKind {
  case enumLike(EnumTypeInfo)
  case structLike(StructTypeInfo)
}

public struct EnumTypeInfo {
  var isObjC: Bool
  var cases: [CaseInfo]
}

public struct CaseInfo {
  var name: String
  var associatedValues: [String?]
}

public struct StructTypeInfo {
  var properties: [StoredProperty]
}

public struct StoredProperty {
  var name: String
  var typeName: String
  var isVar: Bool
  var isStatic: Bool
}
