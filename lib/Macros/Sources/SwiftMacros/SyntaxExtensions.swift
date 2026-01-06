//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Common Syntax extensions used by standard library macros.                  //
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics

extension DeclGroupSyntax {
  internal var accessControlModifiers: DeclModifierListSyntax {
      modifiers.filter { modifier in
      modifier.isAccessControl
    }
  }
}

extension VariableDeclSyntax {
  internal var accessControlModifiers: DeclModifierListSyntax {
    modifiers.filter { modifier in
      modifier.isAccessControl
    }
  }
}

extension ImplicitlyUnwrappedOptionalTypeSyntax {
  internal var asOptionalTypeSyntax: any TypeSyntaxProtocol {
    OptionalTypeSyntax(
      leadingTrivia: leadingTrivia,
      unexpectedBeforeWrappedType,
      wrappedType: wrappedType,
      self.unexpectedBetweenWrappedTypeAndExclamationMark,
      self.unexpectedAfterExclamationMark,
      trailingTrivia: self.trailingTrivia
    )
  }
}

extension LabeledExprListSyntax {
  /// Retrieve the first element with the given label.
  func first(labeled name: String) -> Element? {
    return first { element in
      if let label = element.label, label.text == name {
        return true
      }

      return false
    }
  }
}
