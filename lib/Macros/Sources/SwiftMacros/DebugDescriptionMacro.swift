//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics
import _StringProcessing

public enum DebugDescriptionMacro {}
public enum _DebugDescriptionPropertyMacro {}

/// The member role is used only to perform diagnostics. The member role ensures any diagnostics are emitted once per
/// type. The macro's core behavior begins with the `MemberAttributeMacro` conformance.
extension DebugDescriptionMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  )
  throws -> [DeclSyntax]
  {
    guard !declaration.is(ProtocolDeclSyntax.self) else {
      let message: ErrorMessage = "cannot be attached to a protocol"
      context.diagnose(node: node, error: message)
      return []
    }

    guard declaration.asProtocol(WithGenericParametersSyntax.self)?.genericParameterClause == nil else {
      let message: ErrorMessage = "cannot be attached to a generic definition"
      context.diagnose(node: node, error: message)
      return []
    }

    return []
  }
}

/// A macro which orchestrates conversion of a description property to an LLDB type summary.
///
/// The process of conversion is split across multiple macros/roles. This role performs some analysis on the attached
/// type, and then delegates to `@_DebugDescriptionProperty` to perform the conversion step.
extension DebugDescriptionMacro: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingAttributesFor member: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  )
  throws -> [AttributeSyntax]
  {
    guard !declaration.is(ProtocolDeclSyntax.self) else {
      // Diagnostics for this case are emitted by the `MemberMacro` conformance.
      return []
    }

    guard declaration.asProtocol(WithGenericParametersSyntax.self)?.genericParameterClause == nil else {
      // Diagnostics for this case are emitted by the `MemberMacro` conformance.
      return []
    }

    guard let typeName = declaration.concreteTypeName else {
      let message: ErrorMessage = "cannot be attached to a \(declaration.kind.declName)"
      context.diagnose(node: node, error: message)
      return []
    }

    guard let propertyName = member.as(VariableDeclSyntax.self)?.bindings.only?.name else {
      return []
    }

    guard DESCRIPTION_PROPERTIES.contains(propertyName) else {
      return []
    }

    var properties: [String: PatternBindingSyntax] = [:]
    for member in declaration.memberBlock.members {
      for binding in member.decl.as(VariableDeclSyntax.self)?.bindings ?? [] {
        if let name = binding.name {
          properties[name] = binding
        }
      }
    }

    // Skip if this description property is not prioritized.
    guard propertyName == designatedProperty(properties) else {
      return []
    }

    guard let moduleName = context.moduleName(of: declaration) else {
      // Assertion as a diagnostic.
      let message: ErrorMessage = "could not determine module name from fileID (internal error)"
      context.diagnose(node: declaration, error: message)
      return []
    }

    // Warning: To use a backslash escape in `typeIdentifier`, it needs to be double escaped. This is because
    // the string is serialized to a String literal (an argument to `@_DebugDescriptionProperty`), which
    // effectively "consumes" one level of escaping. To avoid mistakes, dots are matched with `[.]` instead
    // of the more conventional `\.`.
    var typeIdentifier: String
    if let typeParameters = declaration.asProtocol(WithGenericParametersSyntax.self)?.genericParameterClause?.parameters, typeParameters.count > 0 {
      let typePatterns = Array(repeating: ".+", count: typeParameters.count).joined(separator: ",")
      // A regex matching that matches the generic type.
      typeIdentifier = "^\(moduleName)[.]\(typeName)<\(typePatterns)>"
    } else if declaration.is(ExtensionDeclSyntax.self) {
      // When attached to an extension, the type may or may not be a generic type.
      // This regular expression handles both cases.
      typeIdentifier = "^\(moduleName)[.]\(typeName)(<.+>)?$"
    } else {
      typeIdentifier = "\(moduleName).\(typeName)"
    }

    let computedProperties = properties.values.filter(\.isComputedProperty).compactMap(\.name)

    return ["@_DebugDescriptionProperty(\"\(raw: typeIdentifier)\", \(raw: computedProperties))"]
  }
}

/// An internal macro which performs which converts compatible description implementations to an LLDB type
/// summary.
///
/// The LLDB type summary record is emitted into a custom section, which LLDB loads from at debug time.
///
/// Conversion has limitations, primarily that expression evaluation is not supported. If a description
/// property calls another function, it cannot be converted. When conversion cannot be performed, an error
/// diagnostic is emitted.
///
/// Note: There is one ambiguous case: computed properties. The macro can identify some, but not all, uses of
/// computed properties. When a computed property cannot be identified at compile time, LLDB will emit a
/// warning at debug time.
///
/// See https://lldb.llvm.org/use/variable.html#type-summary
extension _DebugDescriptionPropertyMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  )
  throws -> [DeclSyntax]
  {
    guard let arguments = node.arguments else {
      // Assertion as a diagnostic.
      let message: ErrorMessage = "no arguments given to _DebugDescriptionProperty (internal error)"
      context.diagnose(node: node, error: message)
      return []
    }

    guard case .argumentList(let argumentList) = arguments else {
      // Assertion as a diagnostic.
      let message: ErrorMessage = "unexpected arguments to _DebugDescriptionProperty (internal error)"
      context.diagnose(node: arguments, error: message)
      return []
    }

    let argumentExprs = argumentList.map(\.expression)
    guard argumentExprs.count == 2,
          let typeIdentifier = String(expr: argumentExprs[0]),
          let computedProperties = Array<String>(expr: argumentExprs[1]) else {
      // Assertion as a diagnostic.
      let message: ErrorMessage = "incorrect arguments to _DebugDescriptionProperty (internal error)"
      context.diagnose(node: argumentList, error: message)
      return []
    }

    guard let onlyBinding = declaration.as(VariableDeclSyntax.self)?.bindings.only else {
      // Assertion as a diagnostic.
      let message: ErrorMessage = "invalid declaration of _DebugDescriptionProperty (internal error)"
      context.diagnose(node: declaration, error: message)
      return []
    }

    // Validate the body of the description function.
    //   1. The code block must have a single item
    //   2. The single item must be a return of a string literal
    //   3. Later on, the interpolation in the string literal will be validated.
    guard let codeBlock = onlyBinding.accessorBlock?.accessors.as(CodeBlockItemListSyntax.self),
          let descriptionString = codeBlock.asSingleReturnExpr?.as(StringLiteralExprSyntax.self) else {
      let message: ErrorMessage = "body must consist of a single string literal"
      context.diagnose(node: declaration, error: message)
      return []
    }

    // LLDB syntax is not allowed in debugDescription/description.
    let allowLLDBSyntax = onlyBinding.name == "lldbDescription"

    // Iterate the string's segments, and convert property expressions into LLDB variable references.
    var summarySegments: [String] = []
    for segment in descriptionString.segments {
      switch segment {
      case let .stringSegment(segment):
        var literal = segment.content.text
        if !allowLLDBSyntax {
          // To match debugDescription/description, escape `$` characters. LLDB must treat them as a literals they are.
          literal = literal.escapedForLLDB()
        }
        summarySegments.append(literal)
      case let .expressionSegment(segment):
        guard let onlyLabeledExpr = segment.expressions.only, onlyLabeledExpr.label == nil else {
          // This catches `appendInterpolation` overrides.
          let message: ErrorMessage = "unsupported custom string interpolation expression"
          context.diagnose(node: segment, error: message)
          return []
        }

        let expr = onlyLabeledExpr.expression

        // "Parse" the expression into a flattened chain of property accesses.
        var propertyChain: [DeclReferenceExprSyntax]
        do {
          propertyChain = try expr.propertyChain()
        } catch let error as UnexpectedExpr {
          let message: ErrorMessage = "only references to stored properties are allowed"
          context.diagnose(node: error.expr, error: message)
          return []
        }

        // Eliminate explicit self references. The debugger doesn't support `self` in
        // variable paths.
        propertyChain.removeAll(where: { $0.baseName.tokenKind == .keyword(.self) })

        // Check that the root property is not a computed property of `self`. Ideally, all
        // properties would be verified, but a macro expansion has limited scope.
        guard let rootProperty = propertyChain.first else {
          return []
        }

        guard !computedProperties.contains(where: { $0 == rootProperty.baseName.text }) else {
          let message: ErrorMessage = "cannot reference computed properties"
          context.diagnose(node: rootProperty, error: message)
          return []
        }

        let propertyPath = propertyChain.map(\.baseName.text).joined(separator: ".")
        summarySegments.append("${var.\(propertyPath)}")
      @unknown default:
        let message: ErrorMessage = "unexpected string literal segment"
        context.diagnose(node: segment, error: message)
        return []
      }
    }

    let summaryString = summarySegments.joined()

    // Serialize the type summary into a global record, in a custom section, for LLDB to load.
    let decl: DeclSyntax = """
        #if !os(Windows)
        #if os(Linux)
        @_section(".lldbsummaries")
        #else
        @_section("__TEXT,__lldbsummaries")
        #endif
        @_used
        static let _lldb_summary = (
            \(raw: encodeTypeSummaryRecord(typeIdentifier, summaryString))
        )
        #endif
        """

    return [decl]
  }
}

/// The names of properties that can be converted to LLDB type summaries, in priority order.
fileprivate let DESCRIPTION_PROPERTIES = [
  "lldbDescription",
  "debugDescription",
  "description",
]

/// Identifies the prioritized description property, of available properties.
fileprivate func designatedProperty(_ properties: [String: PatternBindingSyntax]) -> String? {
  for name in DESCRIPTION_PROPERTIES {
    if properties[name] != nil {
      return name
    }
  }
  return nil
}

// MARK: - Encoding

fileprivate let ENCODING_VERSION: UInt = 1

/// Construct an LLDB type summary record.
///
/// The record is serialized as a tuple of `UInt8` bytes.
///
/// The record contains the following:
///   * Version number of the record format
///   * The size of the record (encoded as ULEB)
///   * The type identifier, which is either a type name, or for generic types a type regex
///   * The description string converted to an LLDB summary string
///
/// The strings (type identifier and summary) are encoded with both a length prefix (also ULEB)
/// and with a null terminator.
fileprivate func encodeTypeSummaryRecord(_ typeIdentifier: String, _ summaryString: String) -> String {
  let encodedIdentifier = typeIdentifier.byteEncoded
  let encodedSummary = summaryString.byteEncoded
  let recordSize = UInt(encodedIdentifier.count + encodedSummary.count)
  return """
    /* version */ \(swiftLiteral: ENCODING_VERSION.ULEBEncoded),
    /* record size */ \(swiftLiteral: recordSize.ULEBEncoded),
    /* "\(typeIdentifier)" */ \(swiftLiteral: encodedIdentifier),
    /* "\(summaryString)" */ \(swiftLiteral: encodedSummary)
    """
}

extension DefaultStringInterpolation {
  /// Generate a _partial_ Swift literal from the given bytes. It is partial in that must be embedded
  /// into some other syntax, specifically as a tuple.
  fileprivate mutating func appendInterpolation(swiftLiteral bytes: [UInt8]) {
    let literalBytes = bytes.map({ "\($0) as UInt8" }).joined(separator: ", ")
    appendInterpolation(literalBytes)
  }
}

extension String {
  /// Encode a string into UTF8 bytes, prefixed by a ULEB length, and suffixed by the null terminator.
  fileprivate var byteEncoded: [UInt8] {
    let size = UInt(self.utf8.count) + 1 // including null terminator
    var bytes: [UInt8] = []
    bytes.append(contentsOf: size.ULEBEncoded)
    bytes.append(contentsOf: self.utf8)
    bytes.append(0) // null terminator
    return bytes
  }
}

extension UInt {
  /// Encode an unsigned integer into ULEB format. See https://en.wikipedia.org/wiki/LEB128
  fileprivate var ULEBEncoded: [UInt8] {
    guard self > 0 else {
      return [0]
    }

    var bytes: [UInt8] = []
    var buffer = self
    while buffer > 0 {
      var byte = UInt8(buffer & 0b0111_1111)
      buffer >>= 7
      if buffer > 0 {
        byte |= 0b1000_0000
      }
      bytes.append(byte)
    }
    return bytes
  }
}

// MARK: - Diagnostics

fileprivate struct ErrorMessage: DiagnosticMessage, ExpressibleByStringInterpolation {
  init(stringLiteral value: String) {
    self.message = value
  }
  var message: String
  var diagnosticID: MessageID { .init(domain: "DebugDescription", id: "DebugDescription")}
  var severity: DiagnosticSeverity { .error }
}

extension MacroExpansionContext {
  fileprivate func diagnose(node: some SyntaxProtocol, error message: ErrorMessage) {
    diagnose(Diagnostic(node: node, message: message))
  }
}

// MARK: - Syntax Tree Helpers

extension MacroExpansionContext {
  /// Determine the module name of the Syntax node, via its fileID.
  /// See https://developer.apple.com/documentation/swift/fileid()
  fileprivate func moduleName(of node: some SyntaxProtocol) -> String? {
    if let fileID = self.location(of: node)?.file.as(StringLiteralExprSyntax.self)?.representedLiteralValue,
       let firstSlash = fileID.firstIndex(of: "/") {
      return String(fileID.prefix(upTo: firstSlash))
    }
    return nil
  }
}

extension DeclGroupSyntax {
  /// The name of the concrete type represented by this `DeclGroupSyntax`.
  /// This excludes protocols, which return nil.
  fileprivate var concreteTypeName: String? {
    switch self.kind {
    case .actorDecl, .classDecl, .enumDecl, .structDecl:
      return self.asProtocol(NamedDeclSyntax.self)?.name.text
    case .extensionDecl:
      return self.as(ExtensionDeclSyntax.self)?.extendedType.trimmedDescription
    default:
      // New types of decls are not presumed to be valid.
      return nil
    }
  }
}

extension SyntaxKind {
  fileprivate var declName: String {
    var name = String(describing: self)
    name.removeSuffix("Decl")
    return name
  }
}

extension String {
  fileprivate mutating func removeSuffix(_ suffix: String) {
    if self.hasSuffix(suffix) {
      return self.removeLast(suffix.count)
    }
  }
}

extension PatternBindingSyntax {
  /// The property's name.
  fileprivate var name: String? {
    self.pattern.as(IdentifierPatternSyntax.self)?.identifier.text
  }

  /// Predicate which identifies computed properties.
  fileprivate var isComputedProperty: Bool {
    switch self.accessorBlock?.accessors {
    case nil:
      // No accessor block, not computed.
      return false
    case .accessors(let accessors):
      // A `get` accessor indicates a computed property.
      return accessors.contains { $0.accessorSpecifier.tokenKind == .keyword(.get) }
    case .getter:
      // A property with an implementation block is a computed property.
      return true
    @unknown default:
      return true
    }
  }
}

extension CodeBlockItemListSyntax {
  /// The return statement or expression for a code block consisting of only a single item.
  fileprivate var asSingleReturnExpr: ExprSyntax? {
    guard let item = self.only?.item else {
      return nil
    }
    return item.as(ReturnStmtSyntax.self)?.expression ?? item.as(ExprSyntax.self)
  }
}

fileprivate struct UnexpectedExpr: Error {
  let expr: ExprSyntax
}

extension ExprSyntax {
  /// Parse an expression consisting only of property references. Any other syntax throws an error.
  fileprivate func propertyChain() throws -> [DeclReferenceExprSyntax] {
    if let declRef = self.as(DeclReferenceExprSyntax.self) {
      // A reference to a single property on self.
      return [declRef]
    } else if let memberAccess = self.as(MemberAccessExprSyntax.self) {
      return try memberAccess.propertyChain()
    } else {
      // This expression is neither a DeclReference nor a MemberAccess.
      throw UnexpectedExpr(expr: self)
    }
  }
}

extension MemberAccessExprSyntax {
  fileprivate func propertyChain() throws -> [DeclReferenceExprSyntax] {
    // MemberAccess is left associative: a.b.c is ((a.b).c).
    var propertyChain: [DeclReferenceExprSyntax] = []
    var current = self
    while true {
      guard let base = current.base else {
        throw UnexpectedExpr(expr: ExprSyntax(current))
      }

      propertyChain.append(current.declName)

      if let declRef = base.as(DeclReferenceExprSyntax.self) {
        // Terminal case.
        // Top-down traversal produces references in reverse order.
        propertyChain.append(declRef)
        propertyChain.reverse()
        return propertyChain
      } else if let next = base.as(MemberAccessExprSyntax.self) {
        // Recursive case.
        current = next
        continue
      } else {
        // The expression was neither a DeclReference nor a MemberAccess.
        throw UnexpectedExpr(expr: base)
      }
    }
  }
}

extension String {
  /// Convert a StringLiteralExprSyntax to a String.
  fileprivate init?(expr: ExprSyntax) {
    guard let string = expr.as(StringLiteralExprSyntax.self)?.representedLiteralValue else {
      return nil
    }
    self = string
  }
}

extension String {
  fileprivate func escapedForLLDB() -> String {
    guard #available(macOS 13, *) else {
      guard self.firstIndex(of: "$") != nil else {
        return self
      }

      var result = ""
      for char in self {
        if char == "$" {
          result.append("\\$")
        } else {
          result.append(char)
        }
      }
      return result
    }

    return self.replacing("$", with: "\\$")
  }
}

extension Array where Element == String {
  /// Convert an ArrayExprSyntax consisting of StringLiteralExprSyntax to an Array<String>.
  fileprivate init?(expr: ExprSyntax) {
    guard let elements = expr.as(ArrayExprSyntax.self)?.elements else {
      return nil
    }
    self = elements.compactMap { String(expr: $0.expression) }
  }
}

// MARK: - Generic Extensions

extension Collection {
  /// Convert a single element collection to a single value. When a collection consists of
  /// multiple elements, nil is returned.
  fileprivate var only: Element? {
    count == 1 ? first : nil
  }
}
