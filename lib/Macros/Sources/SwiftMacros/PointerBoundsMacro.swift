import SwiftDiagnostics
import SwiftParser
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

protocol ParamInfo: CustomStringConvertible {
    var description: String { get }
    var original: ExprSyntax { get }
    var pointerIndex: Int { get }
    var nonescaping: Bool { get set }

    func getBoundsCheckedThunkBuilder(_ base: BoundsCheckedThunkBuilder, _ funcDecl: FunctionDeclSyntax,
                                  _ variant: Variant
                                 ) -> BoundsCheckedThunkBuilder
}

struct CountedBy: ParamInfo {
    var pointerIndex: Int
    var count: ExprSyntax
    var sizedBy: Bool
    var nonescaping: Bool
    var original: ExprSyntax

    var description: String {
        if sizedBy {
            return ".sizedBy(pointer: \(pointerIndex), size: \"\(count)\", nonescaping: \(nonescaping))"
        }
        return ".countedBy(pointer: \(pointerIndex), count: \"\(count)\", nonescaping: \(nonescaping))"
    }

    func getBoundsCheckedThunkBuilder(_ base: BoundsCheckedThunkBuilder, _ funcDecl: FunctionDeclSyntax,
                                  _ variant: Variant
                                 ) -> BoundsCheckedThunkBuilder {
    let funcParam = getParam(funcDecl, pointerIndex - 1)
    let paramName = funcParam.secondName ?? funcParam.firstName
    let isNullable = funcParam.type.is(OptionalTypeSyntax.self)
    return CountedOrSizedPointerThunkBuilder(base: base, index: pointerIndex - 1, countExpr: count,
        name: paramName, nullable: isNullable, signature: funcDecl.signature, nonescaping: nonescaping, isSizedBy: sizedBy)
    }
}
struct EndedBy: ParamInfo {
    var pointerIndex: Int
    var endIndex: Int
    var nonescaping: Bool
    var original: ExprSyntax

    var description: String {
        return ".endedBy(start: \(pointerIndex), end: \(endIndex), nonescaping: \(nonescaping))"
    }

    func getBoundsCheckedThunkBuilder(_ base: BoundsCheckedThunkBuilder, _ funcDecl: FunctionDeclSyntax,
                                  _ variant: Variant
                                 ) -> BoundsCheckedThunkBuilder {
    let funcParam = getParam(funcDecl, pointerIndex - 1)
    let paramName = funcParam.secondName ?? funcParam.firstName
    let isNullable = funcParam.type.is(OptionalTypeSyntax.self)
    return EndedByPointerThunkBuilder(base: base, startIndex: pointerIndex - 1, endIndex: endIndex - 1,
        name: paramName, nullable: isNullable, signature: funcDecl.signature, nonescaping: nonescaping)
    }
}

struct RuntimeError: Error {
    let description: String

    init(_ description: String) {
        self.description = description
    }

    var errorDescription: String? {
        description
    }
}

struct DiagnosticError: Error {
    let description: String
    let node: SyntaxProtocol
    let notes: [Note]

    init(_ description: String, node: SyntaxProtocol, notes: [Note] = []) {
        self.description = description
        self.node = node
        self.notes = notes
    }

    var errorDescription: String? {
        description
    }
}

enum UnsafePointerKind {
    case Immutable
    case Mutable
}

func transformType(_ prev: TypeSyntax, _ variant: Variant, _ isSizedBy: Bool) throws -> TypeSyntax {
    if let optType = prev.as(OptionalTypeSyntax.self) {
        return TypeSyntax(optType.with(\.wrappedType, try transformType(optType.wrappedType, variant, isSizedBy)))
    }
    guard let idType = prev.as(IdentifierTypeSyntax.self) else {
        throw DiagnosticError("expected pointer type, got \(prev) with kind \(prev.kind)", node: prev)
    }
    let text = idType.name.text
    let kind: UnsafePointerKind = switch text {
    case "UnsafePointer":           .Immutable
    case "UnsafeMutablePointer":    .Mutable
    case "UnsafeRawPointer":        .Immutable
    case "UnsafeMutableRawPointer": .Mutable
    default: throw DiagnosticError("expected Unsafe[Mutable][Raw]Pointer type for type \(prev)" +
        " - first type token is '\(text)'", node: idType.name)
    }
    if isSizedBy {
        let token: TokenSyntax = switch (kind, variant.generateSpan) {
        case (.Immutable, true):  "RawSpan"
        case (.Mutable,   true):  "MutableRawSpan"
        case (.Immutable, false): "UnsafeRawBufferPointer"
        case (.Mutable,   false): "UnsafeMutableRawBufferPointer"
        }
        return TypeSyntax(IdentifierTypeSyntax(name: token))
    }
    if text == "UnsafeRawPointer" || text == "UnsafeMutableRawPointer" {
        throw DiagnosticError("raw pointers only supported for SizedBy", node: idType.name)
    }
    let token: TokenSyntax = switch (kind, variant.generateSpan) {
    case (.Immutable, true):  "Span"
    case (.Mutable,   true):  "MutableSpan"
    case (.Immutable, false): "UnsafeBufferPointer"
    case (.Mutable,   false): "UnsafeMutableBufferPointer"
    }
    return TypeSyntax(idType.with(\.name, token))
}

struct Variant {
    public let generateSpan: Bool
    public let skipTrivialCount: Bool
}

protocol BoundsCheckedThunkBuilder {
    func buildFunctionCall(_ pointerArgs: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax
    func buildBoundsChecks(_ variant: Variant) throws -> [CodeBlockItemSyntax.Item]
    func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws -> FunctionSignatureSyntax
}

func getParam(_ signature: FunctionSignatureSyntax, _ paramIndex: Int) -> FunctionParameterSyntax {
    let params = signature.parameterClause.parameters
    let index = if paramIndex > 0 {
        params.index(params.startIndex, offsetBy: paramIndex)
    } else {
        params.startIndex
    }
    return params[index]
}
func getParam(_ funcDecl: FunctionDeclSyntax, _ paramIndex: Int) -> FunctionParameterSyntax {
    return getParam(funcDecl.signature, paramIndex)
}

struct FunctionCallBuilder: BoundsCheckedThunkBuilder {
    let base: FunctionDeclSyntax
    init(_ function: FunctionDeclSyntax) {
        base = function
    }

    func buildBoundsChecks(_ variant: Variant) throws -> [CodeBlockItemSyntax.Item] {
        return []
    }

    func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws -> FunctionSignatureSyntax {
        var newParams = base.signature.parameterClause.parameters.enumerated().filter {
            let type = argTypes[$0.offset]
            // filter out deleted parameters, i.e. ones where argTypes[i] _contains_ nil
            return type == nil || type! != nil
        }.map { (i: Int, e: FunctionParameterSyntax) in
            e.with(\.type, (argTypes[i] ?? e.type)!)
        }
        let last = newParams.popLast()!
        newParams.append(last.with(\.trailingComma, nil))

        return base.signature.with(\.parameterClause.parameters, FunctionParameterListSyntax(newParams))
    }

    func buildFunctionCall(_ pointerArgs: [Int: ExprSyntax], _: Variant) throws -> ExprSyntax {
        let functionRef = DeclReferenceExprSyntax(baseName: base.name)
        let args: [ExprSyntax] = base.signature.parameterClause.parameters.enumerated()
        .map { (i: Int, param: FunctionParameterSyntax) in
            let name = param.secondName ?? param.firstName
            let declref = DeclReferenceExprSyntax(baseName: name)
            return pointerArgs[i] ?? ExprSyntax(declref)
        }
        let labels: [TokenSyntax?] = base.signature.parameterClause.parameters.map { param in
            let firstName = param.firstName
            if firstName.trimmed.text == "_" {
                return nil
            }
            return firstName
        }
        let labeledArgs: [LabeledExprSyntax] = zip(labels, args).enumerated().map { (i, e) in
            let (label, arg) = e
            var comma: TokenSyntax? = nil
            if i < args.count - 1 {
                comma = .commaToken()
            }
            return LabeledExprSyntax(label: label, expression: arg, trailingComma: comma)
        }
        return ExprSyntax(FunctionCallExprSyntax(calledExpression: functionRef, leftParen: .leftParenToken(),
                                      arguments: LabeledExprListSyntax(labeledArgs), rightParen: .rightParenToken()))
    }
}

func hasReturnType(_ signature: FunctionSignatureSyntax) -> Bool {
    let returnType = signature.returnClause?.type.as(IdentifierTypeSyntax.self)?.name.text ?? "Void"
    return returnType != "Void"
}

protocol PointerBoundsThunkBuilder: BoundsCheckedThunkBuilder {
    var name: TokenSyntax { get }
    var nullable: Bool { get }
    var signature: FunctionSignatureSyntax { get }
    var nonescaping: Bool { get }
}

struct CountedOrSizedPointerThunkBuilder: PointerBoundsThunkBuilder {
    public let base: BoundsCheckedThunkBuilder
    public let index: Int
    public let countExpr: ExprSyntax
    public let name: TokenSyntax
    public let nullable: Bool
    public let signature: FunctionSignatureSyntax
    public let nonescaping: Bool
    public let isSizedBy: Bool

    func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws -> FunctionSignatureSyntax {
        var types = argTypes
        let param = getParam(signature, index)
        types[index] = try transformType(param.type, variant, isSizedBy)
        if variant.skipTrivialCount {
            if let countVar = countExpr.as(DeclReferenceExprSyntax.self) {
                let i = try getParameterIndexForDeclRef(signature.parameterClause.parameters, countVar)
                types[i] = nil as TypeSyntax?
            }
        }
        return try base.buildFunctionSignature(types, variant)
    }

    func buildBoundsChecks(_ variant: Variant) throws -> [CodeBlockItemSyntax.Item] {
        var res = try base.buildBoundsChecks(variant)
        let countName: TokenSyntax = "_\(raw: name)Count"
        let count: VariableDeclSyntax = try VariableDeclSyntax("let \(countName): some BinaryInteger = \(countExpr)")
        res.append(CodeBlockItemSyntax.Item(count))

        let countCheck = ExprSyntax("""
        if \(getCount(variant)) < \(countName) || \(countName) < 0 {
            fatalError("bounds check failure when calling unsafe function")
        }
        """)
        res.append(CodeBlockItemSyntax.Item(countCheck))
        return res
    }

    func unwrapIfNullable(_ expr: ExprSyntax) -> ExprSyntax {
        if nullable {
            return ExprSyntax(ForceUnwrapExprSyntax(expression: expr))
        }
        return expr
    }

    func unwrapIfNonnullable(_ expr: ExprSyntax) -> ExprSyntax {
        if !nullable {
            return ExprSyntax(ForceUnwrapExprSyntax(expression: expr))
        }
        return expr
    }

    func castIntToTargetType(expr: ExprSyntax, type: TypeSyntax) -> ExprSyntax {
        let idType = type.as(IdentifierTypeSyntax.self)!
        if idType.name.text == "Int" {
            return expr
        }
        return ExprSyntax("\(type)(exactly: \(expr))!")
    }

    func buildUnwrapCall(_ argOverrides: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax {
        let unwrappedName = TokenSyntax("_\(name)Ptr")
        var args = argOverrides
        let argExpr = ExprSyntax("\(unwrappedName).baseAddress")
        assert(args[index] == nil)
        args[index] = unwrapIfNonnullable(argExpr)
        let call = try base.buildFunctionCall(args, variant)
        let ptrRef = unwrapIfNullable(ExprSyntax(DeclReferenceExprSyntax(baseName: name)))

        let returnKw: String = if hasReturnType(signature) {
            "return "
        } else {
            ""
        }
        let funcName = isSizedBy ? "withUnsafeBytes" : "withUnsafeBufferPointer"
        let unwrappedCall = ExprSyntax("""
        \(ptrRef).\(raw: funcName) { \(unwrappedName) in
            \(raw: returnKw)\(call)
        }
        """)
        return unwrappedCall
    }

    func getCount(_ variant: Variant) -> ExprSyntax {
        let countName = isSizedBy && variant.generateSpan ? "byteCount" : "count"
        return if nullable {
            ExprSyntax("\(name)?.\(raw: countName) ?? 0")
        } else {
            ExprSyntax("\(name).\(raw: countName)")
        }
    }

    func getPointerArg() -> ExprSyntax {
        return if nullable {
            ExprSyntax("\(name)?.baseAddress")
        } else {
            ExprSyntax("\(name).baseAddress!")
        }
    }

    func buildFunctionCall(_ argOverrides: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax {
        var args = argOverrides
        if variant.skipTrivialCount {
            assert(countExpr.is(DeclReferenceExprSyntax.self) || countExpr.is(IntegerLiteralExprSyntax.self))
            if let countVar = countExpr.as(DeclReferenceExprSyntax.self) {
                let i = try getParameterIndexForDeclRef(signature.parameterClause.parameters, countVar)
                assert(args[i] == nil)
                args[i] = castIntToTargetType(expr: getCount(variant), type: getParam(signature, i).type)
            }
        }
        assert(args[index] == nil)
        if variant.generateSpan {
            assert(nonescaping)
            let unwrappedCall = try buildUnwrapCall(args, variant)
            if nullable {
                var nullArgs = args
                nullArgs[index] = ExprSyntax(NilLiteralExprSyntax(nilKeyword: .keyword(.nil)))
                return ExprSyntax("""
                if \(name) == nil {
                    \(try base.buildFunctionCall(nullArgs, variant))
                } else {
                    \(unwrappedCall)
                }
                """)
            }
            return unwrappedCall
        }

        args[index] = getPointerArg()
        return try base.buildFunctionCall(args, variant)
    }
}

struct EndedByPointerThunkBuilder: PointerBoundsThunkBuilder {
    public let base: BoundsCheckedThunkBuilder
    public let startIndex: Int
    public let endIndex: Int
    public let name: TokenSyntax
    public let nullable: Bool
    public let signature: FunctionSignatureSyntax
    public let nonescaping: Bool

    func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws -> FunctionSignatureSyntax {
        throw RuntimeError("endedBy support not yet implemented")
    }

    func buildBoundsChecks(_ variant: Variant) throws -> [CodeBlockItemSyntax.Item] {
        throw RuntimeError("endedBy support not yet implemented")
    }

    func buildFunctionCall(_ argOverrides: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax {
        throw RuntimeError("endedBy support not yet implemented")
    }
}

func getArgumentByName(_ argumentList: LabeledExprListSyntax, _ name: String) throws -> ExprSyntax {
    guard let arg = argumentList.first(where: {
        return $0.label?.text == name
    }) else {
        throw DiagnosticError("no argument with name '\(name)' in '\(argumentList)'", node: argumentList)
    }
    return arg.expression
}

func getOptionalArgumentByName(_ argumentList: LabeledExprListSyntax, _ name: String) -> ExprSyntax? {
    return argumentList.first(where: {
        $0.label?.text == name
    })?.expression
}

func getParameterIndexForDeclRef(_ parameterList: FunctionParameterListSyntax, _ ref: DeclReferenceExprSyntax) throws -> Int {
    let name = ref.baseName.text
    guard let index = parameterList.enumerated().first(where: { (_: Int, param: FunctionParameterSyntax) in
        let paramenterName = param.secondName ?? param.firstName
        return paramenterName.trimmed.text == name
    })?.offset else {
        throw DiagnosticError("no parameter with name '\(name)' in '\(parameterList)'", node: ref)
    }
    return index
}

/// A macro that adds safe(r) wrappers for functions with unsafe pointer types.
/// Depends on bounds, escapability and lifetime information for each pointer.
/// Intended to map to C attributes like __counted_by, __ended_by and __no_escape,
/// for automatic application by ClangImporter when the C declaration is annotated
/// appropriately.
public struct PointerBoundsMacro: PeerMacro {
    static func parseEnumName(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> String {
        guard let calledExpr = enumConstructorExpr.calledExpression.as(MemberAccessExprSyntax.self) else {
            throw DiagnosticError("expected PointerParam enum literal as argument, got '\(enumConstructorExpr)'", node: enumConstructorExpr)
        }
        return calledExpr.declName.baseName.text
    }

    static func getIntLiteralValue(_ expr: ExprSyntax) throws -> Int {
        guard let intLiteral = expr.as(IntegerLiteralExprSyntax.self) else {
            throw DiagnosticError("expected integer literal, got '\(expr)'", node: expr)
        }
        guard let res = Int(intLiteral.literal.text) else {
            throw DiagnosticError("expected integer literal, got '\(expr)'", node: expr)
        }
        return res
    }

    static func getBoolLiteralValue(_ expr: ExprSyntax) throws -> Bool {
        guard let boolLiteral = expr.as(BooleanLiteralExprSyntax.self) else {
            throw DiagnosticError("expected boolean literal, got '\(expr)'", node: expr)
        }
        guard let res = Bool(boolLiteral.literal.text) else {
            throw DiagnosticError("expected bool literal, got '\(expr)'", node: expr)
        }
        return res
    }

    static func parseCountedByEnum(_ enumConstructorExpr: FunctionCallExprSyntax, _ signature: FunctionSignatureSyntax) throws -> ParamInfo {
        let argumentList = enumConstructorExpr.arguments
        let pointerParamIndexArg = try getArgumentByName(argumentList, "pointer")
        let pointerParamIndex: Int = try getIntLiteralValue(pointerParamIndexArg)
        let countExprArg = try getArgumentByName(argumentList, "count")
        guard let countExprStringLit = countExprArg.as(StringLiteralExprSyntax.self) else {
            throw DiagnosticError("expected string literal for 'count' parameter, got \(countExprArg)", node: countExprArg)
        }
        let unwrappedCountExpr = ExprSyntax(stringLiteral: countExprStringLit.representedLiteralValue!)
        if let countVar = unwrappedCountExpr.as(DeclReferenceExprSyntax.self) {
            // Perform this lookup here so we can override the position to point to the string literal
            // instead of line 1, column 1
            do {
                _ = try getParameterIndexForDeclRef(signature.parameterClause.parameters, countVar)
            } catch let error as DiagnosticError {
                throw DiagnosticError(error.description, node: countExprStringLit, notes: error.notes)
            }
        }
        return CountedBy(pointerIndex: pointerParamIndex, count: unwrappedCountExpr, sizedBy: false, nonescaping: false, original: ExprSyntax(enumConstructorExpr))
    }

    static func parseSizedByEnum(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> ParamInfo {
        let argumentList = enumConstructorExpr.arguments
        let pointerParamIndexArg = try getArgumentByName(argumentList, "pointer")
        let pointerParamIndex: Int = try getIntLiteralValue(pointerParamIndexArg)
        let sizeExprArg = try getArgumentByName(argumentList, "size")
        guard let sizeExprStringLit = sizeExprArg.as(StringLiteralExprSyntax.self) else {
            throw DiagnosticError("expected string literal for 'size' parameter, got \(sizeExprArg)", node: sizeExprArg)
        }
        let unwrappedCountExpr = ExprSyntax(stringLiteral: sizeExprStringLit.representedLiteralValue!)
        return CountedBy(pointerIndex: pointerParamIndex, count: unwrappedCountExpr, sizedBy: true, nonescaping: false, original: ExprSyntax(enumConstructorExpr))
    }

    static func parseEndedByEnum(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> ParamInfo {
        let argumentList = enumConstructorExpr.arguments
        let startParamIndexArg = try getArgumentByName(argumentList, "start")
        let startParamIndex: Int = try getIntLiteralValue(startParamIndexArg)
        let endParamIndexArg = try getArgumentByName(argumentList, "end")
        let endParamIndex: Int = try getIntLiteralValue(endParamIndexArg)
        let nonescapingExprArg = getOptionalArgumentByName(argumentList, "nonescaping")
        let nonescaping = if nonescapingExprArg != nil {
            try getBoolLiteralValue(nonescapingExprArg!)
        } else {
            false
        }
        return EndedBy(pointerIndex: startParamIndex, endIndex: endParamIndex, nonescaping: nonescaping, original: ExprSyntax(enumConstructorExpr))
    }

    static func parseNonEscaping(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> Int {
        let argumentList = enumConstructorExpr.arguments
        let pointerParamIndexArg = try getArgumentByName(argumentList, "pointer")
        let pointerParamIndex: Int = try getIntLiteralValue(pointerParamIndexArg)
        return pointerParamIndex
    }

    static func parseMacroParam(_ paramAST: LabeledExprSyntax, _ signature: FunctionSignatureSyntax, nonescapingPointers: inout Set<Int>) throws -> ParamInfo? {
        let paramExpr = paramAST.expression
        guard let enumConstructorExpr = paramExpr.as(FunctionCallExprSyntax.self) else {
            throw DiagnosticError("expected PointerParam enum literal as argument, got '\(paramExpr)'", node: paramExpr)
        }
        let enumName = try parseEnumName(enumConstructorExpr)
        switch enumName {
        case "countedBy": return try parseCountedByEnum(enumConstructorExpr, signature)
        case "sizedBy": return try parseSizedByEnum(enumConstructorExpr)
        case "endedBy": return try parseEndedByEnum(enumConstructorExpr)
        case "nonescaping":
            let index = try parseNonEscaping(enumConstructorExpr)
            nonescapingPointers.insert(index)
            return nil
        default: throw DiagnosticError("expected 'countedBy', 'sizedBy', 'endedBy' or 'nonescaping', got '\(enumName)'", node: enumConstructorExpr)
        }
    }

    static func hasSafeVariants(_ parsedArgs: [ParamInfo]) -> Bool {
        return parsedArgs.contains { $0.nonescaping }
    }

    static func hasTrivialCountVariants(_ parsedArgs: [ParamInfo]) -> Bool {
        let countExprs = parsedArgs.compactMap { switch $0 {
        case let c as CountedBy: return c.count
        default: return nil
        }}
        let trivialCounts = countExprs.filter {
            $0.is(DeclReferenceExprSyntax.self) || $0 .is(IntegerLiteralExprSyntax.self)
        }
        // don't generate trivial count variants if there are any non-trivial counts
        if trivialCounts.count < countExprs.count {
            return false
        }
        let countVars = trivialCounts.filter { $0.is(DeclReferenceExprSyntax.self) }
        let distinctCountVars = Set(countVars.map {
            return $0.as(DeclReferenceExprSyntax.self)!.baseName.text
        })
        // don't generate trivial count variants if two count expressions refer to the same parameter
        return countVars.count == distinctCountVars.count
    }

    static func checkArgs(_ args: [ParamInfo], _ funcDecl: FunctionDeclSyntax) throws {
        var argByIndex: [Int: ParamInfo] = [:]
        let paramCount = funcDecl.signature.parameterClause.parameters.count
        try args.forEach { pointerArg in
            let i = pointerArg.pointerIndex
            if i < 1 || i > paramCount {
                let noteMessage = if paramCount > 0 {
                    "function \(funcDecl.name) has parameter indices 1..\(paramCount)"
                } else {
                    "function \(funcDecl.name) has no parameters"
                }
                throw DiagnosticError("pointer index out of bounds", node: pointerArg.original,
                notes: [Note(node: Syntax(funcDecl.name), message: MacroExpansionNoteMessage(noteMessage))])
            }
            if argByIndex[i] != nil {
                throw DiagnosticError("multiple PointerParams referring to parameter with index " +
                "\(i): \(pointerArg) and \(argByIndex[i]!)", node: pointerArg.original)
            }
            argByIndex[i] = pointerArg
        }
    }

    static func setNonescapingPointers(_ args: inout [ParamInfo], _ nonescapingPointers: Set<Int>) {
        for i in 0...args.count - 1 where nonescapingPointers.contains(args[i].pointerIndex) {
            args[i].nonescaping = true
        }
    }

    public static func expansion(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        do {
        guard let funcDecl = declaration.as(FunctionDeclSyntax.self) else {
            throw DiagnosticError("@PointerBounds only works on functions", node: declaration)
        }

        let argumentList = node.arguments!.as(LabeledExprListSyntax.self)!
        var nonescapingPointers = Set<Int>()
        var parsedArgs = try argumentList.compactMap { try parseMacroParam($0, funcDecl.signature, nonescapingPointers: &nonescapingPointers) }
        setNonescapingPointers(&parsedArgs, nonescapingPointers)
        try checkArgs(parsedArgs, funcDecl)
        let baseBuilder = FunctionCallBuilder(funcDecl)

        let variant = Variant(generateSpan: hasSafeVariants(parsedArgs), skipTrivialCount: hasTrivialCountVariants(parsedArgs))

        let builder: BoundsCheckedThunkBuilder = parsedArgs.reduce(baseBuilder, { (prev, parsedArg) in
            parsedArg.getBoundsCheckedThunkBuilder(prev, funcDecl, variant)
        })
        let newSignature = try builder.buildFunctionSignature([:], variant)
        let checks = if variant.skipTrivialCount {
            [] as [CodeBlockItemSyntax]
        } else {
            try builder.buildBoundsChecks(variant).map { e in
                CodeBlockItemSyntax(leadingTrivia: "\n", item: e)
            }
        }
        let call = if hasReturnType(funcDecl.signature) {
            CodeBlockItemSyntax(item: CodeBlockItemSyntax.Item(ReturnStmtSyntax(returnKeyword: .keyword(.return, trailingTrivia: " "),
                expression: try builder.buildFunctionCall([:], variant))))
        } else {
            CodeBlockItemSyntax(item: CodeBlockItemSyntax.Item(try builder.buildFunctionCall([:], variant)))
        }
        let body = CodeBlockSyntax(statements: CodeBlockItemListSyntax(checks + [call]))
        let newFunc = funcDecl
            .with(\.signature, newSignature)
            .with(\.body, body)
            .with(\.attributes, funcDecl.attributes.filter { e in
                switch e {
                case .attribute(let attr):
                    // don't apply this macro recursively, and avoid dupe _alwaysEmitIntoClient
                    let name = attr.attributeName.as(IdentifierTypeSyntax.self)?.name.text
                    return name == nil || (name != "PointerBounds" && name != "_alwaysEmitIntoClient")
                default: return true
                }
            } + [.attribute(AttributeSyntax(atSign: .atSignToken(),
                    attributeName: IdentifierTypeSyntax(name: "_alwaysEmitIntoClient")))])
        return [DeclSyntax(newFunc)]
        } catch let error as DiagnosticError {
            context.diagnose(Diagnostic(node: error.node, message: MacroExpansionErrorMessage(error.description),
            notes: error.notes))
            return []
        }
    }
}

