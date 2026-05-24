//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax

/// Access level keywords that can appear on declarations.
enum AccessLevel: Comparable {
  case `private`
  case `fileprivate`
  case `internal`
  case package
  case `public`
  case open
}

/// Extract the explicit access level modifier from a declaration's modifier list.
///
/// Returns `nil` if no explicit access level modifier is present (meaning
/// the declaration has implicit `internal` access).
func accessLevel(of modifiers: DeclModifierListSyntax) -> AccessLevel? {
  for modifier in modifiers {
    switch modifier.name.tokenKind {
    case .keyword(.private):
      return .private
    case .keyword(.fileprivate):
      return .fileprivate
    case .keyword(.internal):
      return .internal
    case .keyword(.package):
      return .package
    case .keyword(.public):
      return .public
    case .keyword(.open):
      return .open
    default:
      continue
    }
  }
  return nil
}

/// Check whether an attribute list contains an attribute with the given name.
func hasAttribute(in attributes: AttributeListSyntax, named name: String) -> Bool {
  for element in attributes {
    guard case .attribute(let attr) = element else {
      continue
    }
    if let identifierType = attr.attributeName.as(IdentifierTypeSyntax.self) {
      if identifierType.name.text == name {
        return true
      }
    }
  }
  return false
}

/// Attributes that indicate a function body must be preserved in the interface
/// because it is part of the ABI or needed for inlining.
private let bodyPreservingAttributes: Set<String> = [
  "inlinable",
  "_transparent",
  "_alwaysEmitIntoClient",
  "backDeployed",
]

/// Determine whether a declaration's body should be preserved in the minimized
/// interface output. Returns `true` for `@inlinable`, `@_transparent`,
/// `@_alwaysEmitIntoClient`, and `@backDeployed` declarations.
func shouldPreserveBody(attributes: AttributeListSyntax) -> Bool {
  for name in bodyPreservingAttributes {
    if hasAttribute(in: attributes, named: name) {
      return true
    }
  }
  return false
}

/// Check whether a declaration has an explicit access-level modifier keyword.
func hasExplicitAccessModifier(_ modifiers: DeclModifierListSyntax) -> Bool {
  return accessLevel(of: modifiers) != nil
}

/// Determine whether a declaration should be kept in the minimized interface.
///
/// Keeps `public`, `open`, `package`, `private`, `@usableFromInline`, and
/// `@_spi` declarations. Strips `internal` and `fileprivate` declarations.
func shouldKeepDecl(
  modifiers: DeclModifierListSyntax,
  attributes: AttributeListSyntax
) -> Bool {
  if hasAttribute(in: attributes, named: "usableFromInline") {
    return true
  }

  if hasAttribute(in: attributes, named: "_spi") {
    return true
  }

  let level = accessLevel(of: modifiers)
  switch level {
  case .public, .open, .package, .private:
    return true
  case .fileprivate:
    return false
  case .internal, nil:
    return false
  }
}
