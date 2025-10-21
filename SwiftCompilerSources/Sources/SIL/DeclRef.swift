//===--- DeclRef.swift ----------------------------------------------------===//
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
import AST

/// A key for referencing an AST declaration in SIL.
///
/// In addition to the AST reference, there are discriminators for referencing different
/// implementation-level entities associated with a single language-level declaration,
/// such as the allocating and initializing entry points of a constructor, etc.
public struct DeclRef: CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedDeclRef

  public var location: Location { Location(bridged: bridged.getLocation()) }

  public var description: String { String(taking: bridged.getDebugDescription()) }

  public var decl: Decl { bridged.getDecl().decl }

  public static func ==(lhs: DeclRef, rhs: DeclRef) -> Bool {
    lhs.bridged.isEqualTo(rhs.bridged)
  }

  /// Do we have enough information to determine all callees that could
  /// be reached by calling the function represented by Decl?
  public func calleesAreStaticallyKnowable(_ context: some Context) -> Bool {
    context._bridged.calleesAreStaticallyKnowable(bridged)
  }
}

extension DeclRef: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    fn(bridged.asDiagnosticArgument())
  }
}
