//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics
import SwiftOperators
import SwiftSyntaxBuilder


extension AttributeSyntax {
  var availability: AttributeSyntax? {
    if attributeName.identifier == "available" {
      return self
    } else {
      return nil
    }
  }
}

extension IfConfigClauseSyntax.Elements {
  var availability: IfConfigClauseSyntax.Elements? {
    switch self {
    case .attributes(let attributes):
      if let availability = attributes.availability {
        return .attributes(availability)
      } else {
        return nil
      }
    default:
      return nil
    }
  }
}

extension IfConfigClauseSyntax {
  var availability: IfConfigClauseSyntax? {
    if let availability = elements?.availability {
      return IfConfigClauseSyntax(
        leadingTrivia: leadingTrivia,
        unexpectedBeforePoundKeyword,
        poundKeyword: poundKeyword,
        unexpectedBetweenPoundKeywordAndCondition,
        condition: condition,
        unexpectedBetweenConditionAndElements,
        elements: availability,
        unexpectedAfterElements,
        trailingTrivia: trailingTrivia
      )
    } else {
      return nil
    }
  }
  
  var clonedAsIf: IfConfigClauseSyntax {
    IfConfigClauseSyntax(
      leadingTrivia: leadingTrivia,
      unexpectedBeforePoundKeyword,
      poundKeyword: .poundIfKeyword(),
      unexpectedBetweenPoundKeywordAndCondition,
      condition: condition,
      unexpectedBetweenConditionAndElements,
      elements: elements,
      unexpectedAfterElements,
      trailingTrivia: trailingTrivia
    )
  }
}

extension IfConfigDeclSyntax {
  var availability: IfConfigDeclSyntax? {
    var elements = [IfConfigClauseListSyntax.Element]()
    for clause in clauses {
      if let availability = clause.availability {
        if elements.isEmpty {
          elements.append(availability.clonedAsIf)
        } else {
          elements.append(availability)
        }
      }
    }
    if elements.isEmpty {
      return nil
    } else {
      return IfConfigDeclSyntax(
        leadingTrivia: leadingTrivia,
        unexpectedBeforeClauses,
        clauses: IfConfigClauseListSyntax(elements),
        unexpectedBetweenClausesAndPoundEndif,
        poundEndif: poundEndif,
        unexpectedAfterPoundEndif,
        trailingTrivia: trailingTrivia
      )
    }
    
  }
}

extension AttributeListSyntax.Element {
  var availability: AttributeListSyntax.Element? {
    switch self {
    case .attribute(let attribute):
      if let availability = attribute.availability {
        return .attribute(availability)
      }
    case .ifConfigDecl(let ifConfig):
      if let availability = ifConfig.availability {
        return .ifConfigDecl(availability)
      }
    default:
      break
    }
    return nil
  }
}

extension AttributeListSyntax {
  var availability: AttributeListSyntax? {
    var elements = [AttributeListSyntax.Element]()
    for element in self {
      if let availability = element.availability {
        elements.append(availability)
      }
    }
    if elements.isEmpty {
      return nil
    }
    return AttributeListSyntax(elements)
  }
}

extension DeclGroupSyntax {
  var availability: AttributeListSyntax? {
    if let attributes {
      return attributes.availability
    } else {
      return nil
    }
  }
}
