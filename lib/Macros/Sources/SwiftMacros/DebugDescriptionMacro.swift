import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics

public struct DebugDescriptionMacro {}

/// Attached peer macro which converts `debugDescription` (or `description`) implementations into
/// an LLDB type summary. The type summary record is emitted into a custom section (via a global
/// constant), where LLDB will load from at debug time.
extension DebugDescriptionMacro: PeerMacro {
    public static func expansion<Decl, Context>(
        of node: AttributeSyntax,
        providingPeersOf declaration: Decl,
        in context: Context
    )
    throws -> [DeclSyntax]
    where Decl: DeclSyntaxProtocol, Context: MacroExpansionContext
    {
        // Determine the module name by doing a bit of parsing on fileID.
        guard let fileID = context.location(of: declaration)?.file,
              let moduleName = extractModuleName(from: fileID) else {
            assertionFailure("could not determine module name from fileID")
            return []
        }

        guard let typeName = identifier(of: declaration)?.text else {
            let message: ErrorMessage = "must be attached to a struct/class/enum/extension"
            context.diagnose(node: node, message: message)
            return []
        }

        // Create a lookup for finding properties by name.
        var properties: [String: PatternBindingSyntax] = [:]
        for member in memberBlock(declaration)?.members ?? [] {
            for binding in member.decl.as(VariableDeclSyntax.self)?.bindings ?? [] {
                if let name = bindingName(binding) {
                    properties[name] = binding
                }
            }
        }

        // Use `debugDescription` if available, otherwise fallback to `description`.
        guard let descriptionProperty = properties["debugDescription"] ?? properties["description"] else {
            let message: ErrorMessage = "debugDescription or description must be defined within \(syntaxKeyword(for: declaration))"
            context.diagnose(node: declaration, message: message)
            return []
        }

        // Validate the body of the description function.
        //   1. The body must have a single item
        //   2. The single item must be a string literal
        //   3. Later on, the interpolation in the string literal will be validated.
        guard let codeBlock = descriptionProperty.accessorBlock?.accessors.as(CodeBlockItemListSyntax.self),
              let descriptionString = codeBlock.single?.item.as(StringLiteralExprSyntax.self) else {
            let message: ErrorMessage = "\(bindingName(descriptionProperty)!) must consist of a single string literal"
            context.diagnose(node: descriptionProperty, message: message)
            return []
        }

        // Precompute which properties are known to be computed. Used for producing diagnostics.
        var computedProperties: Set<String> = []
        for property in properties.values {
            if isComputedProperty(property), let name = bindingName(property) {
                computedProperties.insert(name)
            }
        }

        // Iterate the string's segments, and convert property expressions into LLDB variable references.
        var summarySegments: [String] = []
        for segment in descriptionString.segments {
            switch segment {
            case let .stringSegment(segment):
                summarySegments.append(segment.content.text)
            case let .expressionSegment(segment):
                guard let labeledExpr = segment.expressions.single, labeledExpr.label == nil else {
                    // This catches `appendInterpolation` overrides.
                    let message: ErrorMessage = "unsupported custom string interpolation expression"
                    context.diagnose(node: segment, message: message)
                    return []
                }

                let expr = labeledExpr.expression

                var propertyChain: [String] = []
                if let declRef = expr.as(DeclReferenceExprSyntax.self) {
                    // A reference to a single property on self.
                    propertyChain.append(declRef.baseName.text)
                } else if let memberAccess = expr.as(MemberAccessExprSyntax.self) {
                    // A chain of properties, possibly starting with an explicit self.
                    enumerateMembers(memberAccess) { declRef in
                        propertyChain.append(declRef.baseName.text)
                    }
                } else {
                    // The expression was neither a DeclReference nor a MemberAccess.
                    let message: ErrorMessage = "unsupported expression; stored properties only"
                    context.diagnose(node: expr, message: message)
                    return []
                }

                // Explicit self are removed before use by LLDB.
                if propertyChain[0] == "self" {
                    propertyChain.removeFirst()
                }

                // Check that the root property is not a computed property on `self`.
                // Ideally, all properties would be checked, but a macro expansion can
                // at best only check the properties of the type it's attached to.
                let rootProperty = propertyChain[0]
                guard !computedProperties.contains(rootProperty) else {
                    let message: ErrorMessage = "cannot reference computed properties"
                    context.diagnose(node: expr, message: message)
                    return []
                }

                let propertyPath = propertyChain.joined(separator: ".")
                summarySegments.append("${var.\(propertyPath)}")
            @unknown default:
                return []
            }
        }

        let summaryString = summarySegments.joined()

        var typeIdentifier: String
        if let typeParameters = genericParameters(declaration), typeParameters.count > 0 {
            let typePatterns = Array(repeating: ".+", count: typeParameters.count).joined(separator: ",")
            // A regex matching that matches the generic type.
            typeIdentifier = "^\(moduleName)\\.\(typeName)<\(typePatterns)>"
        } else if declaration.is(ExtensionDeclSyntax.self) {
            // When attached to an extension, the type may or may not be a generic type.
            // This regular expression handles both cases.
            typeIdentifier = "^\(moduleName)\\.\(typeName)(<.+>)?$"
        } else {
            typeIdentifier = "\(moduleName).\(typeName)"
        }

        // Serialize the type summary into a global record, in a custom section, for LLDB to load.
        let decl: DeclSyntax = """
        #if os(Linux)
        @_section(".lldbsummaries")
        #elseif os(Windows)
        @_section(".lldbsummaries")
        #else
        @_section("__DATA_CONST,__lldbsummaries")
        #endif
        @_used
        let \(raw: typeName)_lldb_summary = (
            \(raw: encodeTypeSummaryRecord(typeIdentifier, summaryString))
        )
        """

        return [decl]
    }
}

// MARK: - Diagnostics

private struct ErrorMessage: DiagnosticMessage, ExpressibleByStringInterpolation {
    init(stringLiteral value: String) {
        self.message = value
    }
    var message: String
    var diagnosticID: MessageID { .init(domain: "DebugDescription", id: "DebugDescription")}
    var severity: DiagnosticSeverity { .error }
}

extension MacroExpansionContext {
    fileprivate func diagnose(node: some SyntaxProtocol, message: any DiagnosticMessage) {
        diagnose(Diagnostic(node: node, message: message))
    }
}

// MARK: - AST Helpers

private func extractModuleName(from fileID: ExprSyntax) -> String? {
    if let fileID = fileID.as(StringLiteralExprSyntax.self)?.representedLiteralValue,
       let firstSlash = fileID.firstIndex(of: "/") {
        return String(fileID.prefix(upTo: firstSlash))
    }
    return nil
}

private func identifier<Decl: DeclSyntaxProtocol>(of decl: Decl) -> TokenSyntax? {
    decl.as(StructDeclSyntax.self)?.name ??
    decl.as(ClassDeclSyntax.self)?.name ??
    decl.as(EnumDeclSyntax.self)?.name ??
    decl.as(ExtensionDeclSyntax.self)?.extendedType.as(IdentifierTypeSyntax.self)?.name
}

private func memberBlock<Decl: DeclSyntaxProtocol>(_ decl: Decl) -> MemberBlockSyntax? {
    decl.as(StructDeclSyntax.self)?.memberBlock ??
    decl.as(ClassDeclSyntax.self)?.memberBlock ??
    decl.as(EnumDeclSyntax.self)?.memberBlock ??
    decl.as(ExtensionDeclSyntax.self)?.memberBlock
}

private func syntaxKeyword<Decl: DeclSyntaxProtocol>(for decl: Decl) -> String {
    if decl.is(StructDeclSyntax.self) { return "struct" }
    if decl.is(ClassDeclSyntax.self) { return "class" }
    if decl.is(EnumDeclSyntax.self) { return "enum" }
    if decl.is(ExtensionDeclSyntax.self) { return "extension" }
    assertionFailure("expected struct/class/enum/extension")
    return "declaration"
}

private func genericParameters<Decl: DeclSyntaxProtocol>(_ decl: Decl) -> GenericParameterListSyntax? {
    decl.as(StructDeclSyntax.self)?.genericParameterClause?.parameters ??
    decl.as(ClassDeclSyntax.self)?.genericParameterClause?.parameters ??
    decl.as(EnumDeclSyntax.self)?.genericParameterClause?.parameters
}

private func bindingName(_ binding: PatternBindingSyntax) -> String? {
    binding.pattern.as(IdentifierPatternSyntax.self)?.identifier.text
}

private func isComputedProperty(_ binding: PatternBindingSyntax) -> Bool {
    guard let accessors = binding.accessorBlock?.accessors else {
        // No accessor block, not computed.
        return false
    }

    switch accessors {
    case .accessors(let accessors):
        return accessors.contains { $0.accessorSpecifier.tokenKind == .keyword(.get) }
    case .getter:
        return true
    @unknown default:
        return false
    }
}

// Enumerate a MemberAccess expression to produce DeclReferences in left to right order.
// `a.b.c` will result in callbacks for each DeclReference `a`, `b`, and then `c`.
private func enumerateMembers(_ memberAccess: MemberAccessExprSyntax, _ action: (DeclReferenceExprSyntax) -> Void) {
    if let baseMember = memberAccess.base?.as(MemberAccessExprSyntax.self) {
        enumerateMembers(baseMember, action)
    } else if let baseDecl = memberAccess.base?.as(DeclReferenceExprSyntax.self) {
        action(baseDecl)
    }
    action(memberAccess.declName)
}

// MARK: - Encoding

/// Construct an LLDB type summary record.
///
/// The record is serializeed as a tuple of `UInt8` bytes.
///
/// The record contains the following:
///   * Version number of the record format
///   * The size of the record (encoded as ULEB)
///   * The type identifier, which is either a type name, or for generic types a type regex
///   * The description string converted to an LLDB summary string
///
/// The strings (type identifier and summary) are encoded with both a length prefix (also ULEB)
/// and with a null terminator.
private func encodeTypeSummaryRecord(_ typeIdentifier: String, _ summaryString: String) -> String {
    let encodedType = encodeString(typeIdentifier)
    let encodedSummary = encodeString(summaryString)
    let recordSize = UInt(encodedType.count + encodedSummary.count)
    return """
    /* version */ 1 as UInt8,
    /* record size */ \(literalBytes(encodeULEB(recordSize))),
    /* "\(typeIdentifier)" */ \(literalBytes(encodedType)),
    /* "\(summaryString)" */ \(literalBytes(encodedSummary))
    """
}

/// Generate a _partial_ Swift literal from the given bytes. It is partial in that must be embedded
/// into some other syntax, specifically as a tuple.
private func literalBytes(_ bytes: [UInt8]) -> String {
    bytes.map({ "\($0) as UInt8" }).joined(separator: ", ")
}

/// Encode a string into UTF8 bytes, prefixed by a ULEB length, and suffixed by the null terminator.
private func encodeString(_ string: String) -> [UInt8] {
    let size = UInt(string.utf8.count) + 1 // including null terminator
    var bytes: [UInt8] = []
    bytes.append(contentsOf: encodeULEB(size))
    bytes.append(contentsOf: string.utf8)
    bytes.append(0) // null terminator
    return bytes
}

/// Encode an unsigned integer into ULEB format. See https://en.wikipedia.org/wiki/LEB128
private func encodeULEB(_ value: UInt) -> [UInt8] {
    guard value > 0 else {
        return [0]
    }

    var bytes: [UInt8] = []
    var buffer = value
    while buffer > 0 {
        var byte = UInt8(buffer & 0b01111111)
        buffer >>= 7
        if buffer > 0 {
            byte |= 0b10000000
        }
        bytes.append(byte)
    }
    return bytes
}

// MARK: - Extensions

extension Collection {
    /// Convert a single element collection to a single value. When a collection consists of
    /// multiple elements, nil is returned.
    fileprivate var single: Element? {
        count == 1 ? first : nil
    }
}
