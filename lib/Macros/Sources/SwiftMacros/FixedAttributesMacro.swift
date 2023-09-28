import SwiftDiagnostics
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct FixedAttributesMacro: AttributeMacro {
  // FIXME: Should diagnose errors with the diagnostic engine so we can check all the strings.
  enum ExpansionError: Error {
    case argumentHasLabel
    case argumentNotString
    case argumentHasInterpolations
  }

  static public func expansion(
    of node: AttributeSyntax,
    providingAttributesFor member: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    guard case .argumentList(let list)? = node.arguments else {
      return []
    }

    return try list.map { labeledExpr in
      guard labeledExpr.label == nil else {
        throw ExpansionError.argumentHasLabel
      }
      guard let stringExpr = labeledExpr.expression.as(StringLiteralExprSyntax.self) else {
        throw ExpansionError.argumentNotString
      }
      guard
        stringExpr.segments.count == 1,
        case .stringSegment(let segment)? = stringExpr.segments.first,
        case .stringSegment(let text) = segment.content.tokenKind
      else {
        throw ExpansionError.argumentHasInterpolations
      }

//      var parser = Parser(text)
//      return AttributeSyntax.parse(from: &parser)
      return "\(raw: text)"
    }
  }
}

