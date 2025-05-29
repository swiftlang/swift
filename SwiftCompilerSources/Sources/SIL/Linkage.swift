//===--- Linkage.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SILBridging

public enum Linkage: CustomStringConvertible {
  /// This object definition is visible to multiple Swift modules (and
  /// thus potentially across linkage-unit boundaries).  There are no
  /// other object definitions with this name in the program.
  ///
  /// Public functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  case `public`

  /// This is a special linkage used for symbols which are treated
  /// as public for the purposes of SIL serialization and optimization,
  /// but do not have public entry points in the generated binary.
  ///
  /// This linkage is used for @_alwaysEmitIntoClient functions.
  ///
  /// There is no external variant of this linkage, because from other
  /// translation units in the same module, this behaves identically
  /// to the HiddenExternal linkage.
  ///
  /// When deserialized, such declarations receive Shared linkage.
  ///
  /// PublicNonABI functions must be definitions.
  case publicNonABI

  /// Same as \c Public, except the definition is visible within a package
  /// of modules.
  case package

  /// Similar to \c PublicNonABI, this definition is used for symbols treated
  /// as package but do not have package entry points in the generated binary.
  /// It's used for default argument expressions and `@_alwaysEmitIntoClient`.
  /// When deserialized, this will become \c Shared linkage.
  case packageNonABI

  /// This object definition is visible only to the current Swift
  /// module (and thus should not be visible across linkage-unit
  /// boundaries).  There are no other object definitions with this
  /// name in the module.
  ///
  /// Hidden functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  case hidden

  /// This object definition is visible only within a single Swift
  /// module.  There may be other object definitions with this name in
  /// the module; those definitions are all guaranteed to be
  /// semantically equivalent to this one.
  ///
  /// This linkage is used e.g. for thunks and for specialized functions.
  ///
  /// Shared functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  case shared

  /// This object definition is visible only within a single Swift
  /// file.
  ///
  /// Private functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  case `private`

  /// A Public definition with the same name as this object will be
  /// available to the current Swift module at runtime.  If this
  /// object is a definition, it is semantically equivalent to that
  /// definition.
  case publicExternal

  /// Similar to \c PublicExternal.
  /// Used to reference a \c Package definition in a different module
  /// within a package.
  case packageExternal

  /// A Public or Hidden definition with the same name as this object
  /// will be defined by the current Swift module at runtime.
  ///
  /// This linkage is only used for non-whole-module compilations to refer to
  /// functions in other files of the same module.
  case hiddenExternal

  public var isExternal: Bool {
    switch self {
    case .public,
         .publicNonABI,
         .package,
         .packageNonABI,
         .hidden,
         .shared,
         .private:
      return false
    case .packageExternal,
         .publicExternal,
         .hiddenExternal:
      return true
    }

  }

  public var description: String {
    switch self {
      case .public:          return "public"
      case .publicNonABI:    return "publicNonABI"
      case .package:         return "package"
      case .packageNonABI:   return "packageNonABI"
      case .hidden:          return "hidden"
      case .shared:          return "shared"
      case .private:         return "private"
      case .packageExternal: return "packageExternal"
      case .publicExternal:  return "publicExternal"
      case .hiddenExternal:  return "hiddenExternal"
    }
  }
}

// Bridging utilities

extension BridgedLinkage {
  var linkage: Linkage {
    switch self {
      case .Public:          return .public
      case .PublicNonABI:    return .publicNonABI
      case .Package:         return .package
      case .PackageNonABI:   return .packageNonABI
      case .Hidden:          return .hidden
      case .Shared:          return .shared
      case .Private:         return .private
      case .PublicExternal:  return .publicExternal
      case .PackageExternal: return .packageExternal
      case .HiddenExternal:  return .hiddenExternal
      default:
        fatalError("unsupported argument convention")
    }
  }
}

extension Linkage {
  public var bridged: BridgedLinkage {
    switch self {
      case .public:          return .Public
      case .publicNonABI:    return .PublicNonABI
      case .package:         return .Package
      case .packageNonABI:   return .PackageNonABI
      case .hidden:          return .Hidden
      case .shared:          return .Shared
      case .private:         return .Private
      case .publicExternal:  return .PublicExternal
      case .packageExternal: return .PackageExternal
      case .hiddenExternal:  return .HiddenExternal
    }
  }
}
