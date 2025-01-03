import SwiftDiagnostics
import SwiftParser
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

protocol ParamInfo: CustomStringConvertible {
  var description: String { get }
  var original: SyntaxProtocol { get }
  var pointerIndex: Int { get }
  var nonescaping: Bool { get set }

  func getBoundsCheckedThunkBuilder(
    _ base: BoundsCheckedThunkBuilder, _ funcDecl: FunctionDeclSyntax,
    _ variant: Variant
  ) -> BoundsCheckedThunkBuilder
}

struct CxxSpan: ParamInfo {
  var pointerIndex: Int
  var nonescaping: Bool
  var original: SyntaxProtocol
  var typeMappings: [String: String]

  var description: String {
    return "std::span(pointer: \(pointerIndex), nonescaping: \(nonescaping))"
  }

  func getBoundsCheckedThunkBuilder(
    _ base: BoundsCheckedThunkBuilder, _ funcDecl: FunctionDeclSyntax,
    _ variant: Variant
  ) -> BoundsCheckedThunkBuilder {
    CxxSpanThunkBuilder(base: base, index: pointerIndex - 1, signature: funcDecl.signature, 
      typeMappings: typeMappings, node: original)
  }
}

struct CountedBy: ParamInfo {
  var pointerIndex: Int
  var count: ExprSyntax
  var sizedBy: Bool
  var nonescaping: Bool
  var original: SyntaxProtocol

  var description: String {
    if sizedBy {
      return ".sizedBy(pointer: \(pointerIndex), size: \"\(count)\", nonescaping: \(nonescaping))"
    }
    return ".countedBy(pointer: \(pointerIndex), count: \"\(count)\", nonescaping: \(nonescaping))"
  }

  func getBoundsCheckedThunkBuilder(
    _ base: BoundsCheckedThunkBuilder, _ funcDecl: FunctionDeclSyntax,
    _ variant: Variant
  ) -> BoundsCheckedThunkBuilder {
    let funcParam = getParam(funcDecl, pointerIndex - 1)
    let paramName = funcParam.secondName ?? funcParam.firstName
    let isNullable = funcParam.type.is(OptionalTypeSyntax.self)
    return CountedOrSizedPointerThunkBuilder(
      base: base, index: pointerIndex - 1, countExpr: count,
      name: paramName, nullable: isNullable, signature: funcDecl.signature,
      nonescaping: nonescaping, isSizedBy: sizedBy)
  }
}

struct EndedBy: ParamInfo {
  var pointerIndex: Int
  var endIndex: Int
  var nonescaping: Bool
  var original: SyntaxProtocol

  var description: String {
    return ".endedBy(start: \(pointerIndex), end: \(endIndex), nonescaping: \(nonescaping))"
  }

  func getBoundsCheckedThunkBuilder(
    _ base: BoundsCheckedThunkBuilder, _ funcDecl: FunctionDeclSyntax,
    _ variant: Variant
  ) -> BoundsCheckedThunkBuilder {
    let funcParam = getParam(funcDecl, pointerIndex - 1)
    let paramName = funcParam.secondName ?? funcParam.firstName
    let isNullable = funcParam.type.is(OptionalTypeSyntax.self)
    return EndedByPointerThunkBuilder(
      base: base, startIndex: pointerIndex - 1, endIndex: endIndex - 1,
      name: paramName, nullable: isNullable, signature: funcDecl.signature, nonescaping: nonescaping
    )
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

enum Mutability {
  case Immutable
  case Mutable
}

func getTypeName(_ type: TypeSyntax) throws -> TokenSyntax {
  switch type.kind {
  case .memberType:
    let memberType = type.as(MemberTypeSyntax.self)!
    if !memberType.baseType.isSwiftCoreModule {
      throw DiagnosticError(
        "expected pointer type in Swift core module, got type \(type) with base type \(memberType.baseType)",
        node: type)
    }
    return memberType.name
  case .identifierType:
    return type.as(IdentifierTypeSyntax.self)!.name
  default:
    throw DiagnosticError("expected pointer type, got \(type) with kind \(type.kind)", node: type)
  }
}

func replaceTypeName(_ type: TypeSyntax, _ name: TokenSyntax) -> TypeSyntax {
  if let memberType = type.as(MemberTypeSyntax.self) {
    return TypeSyntax(memberType.with(\.name, name))
  }
  let idType = type.as(IdentifierTypeSyntax.self)!
  return TypeSyntax(idType.with(\.name, name))
}

func getPointerMutability(text: String) -> Mutability? {
  switch text {
  case "UnsafePointer": return .Immutable
  case "UnsafeMutablePointer": return .Mutable
  case "UnsafeRawPointer": return .Immutable
  case "UnsafeMutableRawPointer": return .Mutable
  case "OpaquePointer": return .Immutable
  default:
    return nil
  }
}

func isRawPointerType(text: String) -> Bool {
  switch text {
  case "UnsafeRawPointer": return true
  case "UnsafeMutableRawPointer": return true
  case "OpaquePointer": return true
  default:
    return false
  }
}

func getSafePointerName(mut: Mutability, generateSpan: Bool, isRaw: Bool) -> TokenSyntax {
  switch (mut, generateSpan, isRaw) {
  case (.Immutable, true, true): return "RawSpan"
  case (.Mutable, true, true): return "MutableRawSpan"
  case (.Immutable, false, true): return "UnsafeRawBufferPointer"
  case (.Mutable, false, true): return "UnsafeMutableRawBufferPointer"

  case (.Immutable, true, false): return "Span"
  case (.Mutable, true, false): return "MutableSpan"
  case (.Immutable, false, false): return "UnsafeBufferPointer"
  case (.Mutable, false, false): return "UnsafeMutableBufferPointer"
  }
}

func transformType(_ prev: TypeSyntax, _ variant: Variant, _ isSizedBy: Bool) throws -> TypeSyntax {
  if let optType = prev.as(OptionalTypeSyntax.self) {
    return TypeSyntax(
      optType.with(\.wrappedType, try transformType(optType.wrappedType, variant, isSizedBy)))
  }
  if let impOptType = prev.as(ImplicitlyUnwrappedOptionalTypeSyntax.self) {
    return try transformType(impOptType.wrappedType, variant, isSizedBy)
  }
  let name = try getTypeName(prev)
  let text = name.text
  let isRaw = isRawPointerType(text: text)
  if isRaw && !isSizedBy {
    throw DiagnosticError("raw pointers only supported for SizedBy", node: name)
  }
  if !isRaw && isSizedBy {
    throw DiagnosticError("SizedBy only supported for raw pointers", node: name)
  }

  guard let kind: Mutability = getPointerMutability(text: text) else {
    throw DiagnosticError(
      "expected Unsafe[Mutable][Raw]Pointer type for type \(prev)"
        + " - first type token is '\(text)'", node: name)
  }
  let token = getSafePointerName(mut: kind, generateSpan: variant.generateSpan, isRaw: isSizedBy)
  if isSizedBy {
    return TypeSyntax(IdentifierTypeSyntax(name: token))
  }
  return replaceTypeName(prev, token)
}

struct Variant {
  public let generateSpan: Bool
  public let skipTrivialCount: Bool
}

protocol BoundsCheckedThunkBuilder {
  func buildFunctionCall(_ pointerArgs: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax
  func buildBoundsChecks(_ variant: Variant) throws -> [CodeBlockItemSyntax.Item]
  func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws
    -> FunctionSignatureSyntax
}

func getParam(_ signature: FunctionSignatureSyntax, _ paramIndex: Int) -> FunctionParameterSyntax {
  let params = signature.parameterClause.parameters
  if paramIndex > 0 {
    return params[params.index(params.startIndex, offsetBy: paramIndex)]
  } else {
    return params[params.startIndex]
  }
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

  func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws
    -> FunctionSignatureSyntax
  {
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
      let firstName = param.firstName.trimmed
      if firstName.text == "_" {
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
      let colon: TokenSyntax? = label != nil ? .colonToken() : nil
      return LabeledExprSyntax(label: label, colon: colon, expression: arg, trailingComma: comma)
    }
    return ExprSyntax(
      FunctionCallExprSyntax(
        calledExpression: functionRef, leftParen: .leftParenToken(),
        arguments: LabeledExprListSyntax(labeledArgs), rightParen: .rightParenToken()))
  }
}

struct CxxSpanThunkBuilder: BoundsCheckedThunkBuilder {
  public let base: BoundsCheckedThunkBuilder
  public let index: Int
  public let signature: FunctionSignatureSyntax
  public let typeMappings: [String: String] 
  public let node: SyntaxProtocol

  func buildBoundsChecks(_ variant: Variant) throws -> [CodeBlockItemSyntax.Item] {
    return []
  }

  func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws
    -> FunctionSignatureSyntax {
    var types = argTypes
    let param = getParam(signature, index)
    let typeName = try getTypeName(param.type).text;
    guard let desugaredType = typeMappings[typeName] else {
      throw DiagnosticError(
        "unable to desugar type with name '\(typeName)'", node: node)
    }

    let parsedDesugaredType = try TypeSyntax("\(raw: desugaredType)")
    types[index] = TypeSyntax(IdentifierTypeSyntax(name: "Span",
      genericArgumentClause: parsedDesugaredType.as(IdentifierTypeSyntax.self)!.genericArgumentClause))
    return try base.buildFunctionSignature(types, variant)
  }

  func buildFunctionCall(_ pointerArgs: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax {
    var args = pointerArgs
    let param = getParam(signature, index)
    let typeName = try getTypeName(param.type).text;
    assert(args[index] == nil)
    args[index] = ExprSyntax("\(raw: typeName)(\(raw: param.secondName ?? param.firstName))")
    return try base.buildFunctionCall(args, variant)
  }
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

  func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws
    -> FunctionSignatureSyntax
  {
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
    let count: VariableDeclSyntax = try VariableDeclSyntax(
      "let \(countName): some BinaryInteger = \(countExpr)")
    res.append(CodeBlockItemSyntax.Item(count))

    let countCheck = ExprSyntax(
      """
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
    if type.canRepresentBasicType(type: Int.self) {
      return expr
    }
    return ExprSyntax("\(type)(exactly: \(expr))!")
  }

  func buildUnwrapCall(_ argOverrides: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax {
    let unwrappedName = TokenSyntax("_\(name)Ptr")
    var args = argOverrides
    let argExpr = ExprSyntax("\(unwrappedName).baseAddress")
    assert(args[index] == nil)
    args[index] = try castPointerToOpaquePointer(unwrapIfNonnullable(argExpr))
    let call = try base.buildFunctionCall(args, variant)
    let ptrRef = unwrapIfNullable(ExprSyntax(DeclReferenceExprSyntax(baseName: name)))

    let funcName = isSizedBy ? "withUnsafeBytes" : "withUnsafeBufferPointer"
    let unwrappedCall = ExprSyntax(
      """
        \(ptrRef).\(raw: funcName) { \(unwrappedName) in
          return \(call)
        }
      """)
    return unwrappedCall
  }

  func getCount(_ variant: Variant) -> ExprSyntax {
    let countName = isSizedBy && variant.generateSpan ? "byteCount" : "count"
    if nullable {
      return ExprSyntax("\(name)?.\(raw: countName) ?? 0")
    }
    return ExprSyntax("\(name).\(raw: countName)")
  }

  func peelOptionalType(_ type: TypeSyntax) -> TypeSyntax {
    if let optType = type.as(OptionalTypeSyntax.self) {
      return optType.wrappedType
    }
    if let impOptType = type.as(ImplicitlyUnwrappedOptionalTypeSyntax.self) {
      return impOptType.wrappedType
    }
    return type
  }

  func castPointerToOpaquePointer(_ baseAddress: ExprSyntax) throws -> ExprSyntax {
    let i = try getParameterIndexForParamName(signature.parameterClause.parameters, name)
    let type = peelOptionalType(getParam(signature, i).type)
    if type.canRepresentBasicType(type: OpaquePointer.self) {
      return ExprSyntax("OpaquePointer(\(baseAddress))")
    }
    return baseAddress
  }

  func getPointerArg() throws -> ExprSyntax {
    if nullable {
      return ExprSyntax("\(name)?.baseAddress")
    }
    return ExprSyntax("\(name).baseAddress!")
  }

  func buildFunctionCall(_ argOverrides: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax
  {
    var args = argOverrides
    if variant.skipTrivialCount {
      assert(
        countExpr.is(DeclReferenceExprSyntax.self) || countExpr.is(IntegerLiteralExprSyntax.self))
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
        return ExprSyntax(
          """
            if \(name) == nil {
              \(try base.buildFunctionCall(nullArgs, variant))
            } else {
              \(unwrappedCall)
            }
          """)
      }
      return unwrappedCall
    }

    args[index] = try castPointerToOpaquePointer(getPointerArg())
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

  func buildFunctionSignature(_ argTypes: [Int: TypeSyntax?], _ variant: Variant) throws
    -> FunctionSignatureSyntax
  {
    throw RuntimeError("endedBy support not yet implemented")
  }

  func buildBoundsChecks(_ variant: Variant) throws -> [CodeBlockItemSyntax.Item] {
    throw RuntimeError("endedBy support not yet implemented")
  }

  func buildFunctionCall(_ argOverrides: [Int: ExprSyntax], _ variant: Variant) throws -> ExprSyntax
  {
    throw RuntimeError("endedBy support not yet implemented")
  }
}

func getArgumentByName(_ argumentList: LabeledExprListSyntax, _ name: String) throws -> ExprSyntax {
  guard
    let arg = argumentList.first(where: {
      return $0.label?.text == name
    })
  else {
    throw DiagnosticError(
      "no argument with name '\(name)' in '\(argumentList)'", node: argumentList)
  }
  return arg.expression
}

func getOptionalArgumentByName(_ argumentList: LabeledExprListSyntax, _ name: String) -> ExprSyntax?
{
  return argumentList.first(where: {
    $0.label?.text == name
  })?.expression
}

func getParameterIndexForParamName(
  _ parameterList: FunctionParameterListSyntax, _ tok: TokenSyntax
) throws -> Int {
  let name = tok.text
  guard
    let index = parameterList.enumerated().first(where: {
      (_: Int, param: FunctionParameterSyntax) in
      let paramenterName = param.secondName ?? param.firstName
      return paramenterName.trimmed.text == name
    })?.offset
  else {
    throw DiagnosticError("no parameter with name '\(name)' in '\(parameterList)'", node: tok)
  }
  return index
}

func getParameterIndexForDeclRef(
  _ parameterList: FunctionParameterListSyntax, _ ref: DeclReferenceExprSyntax
) throws -> Int {
  return try getParameterIndexForParamName((parameterList), ref.baseName)
}

/// A macro that adds safe(r) wrappers for functions with unsafe pointer types.
/// Depends on bounds, escapability and lifetime information for each pointer.
/// Intended to map to C attributes like __counted_by, __ended_by and __no_escape,
/// for automatic application by ClangImporter when the C declaration is annotated
/// appropriately. Moreover, it can wrap C++ APIs using unsafe C++ types like
/// std::span with APIs that use their safer Swift equivalents.
public struct SwiftifyImportMacro: PeerMacro {
  static func parseEnumName(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> String {
    guard let calledExpr = enumConstructorExpr.calledExpression.as(MemberAccessExprSyntax.self)
    else {
      throw DiagnosticError(
        "expected _SwiftifyInfo enum literal as argument, got '\(enumConstructorExpr)'",
        node: enumConstructorExpr)
    }
    return calledExpr.declName.baseName.text
  }

  static func getIntLiteralValue(_ expr: ExprSyntax) throws -> Int {
    guard let intLiteral = expr.as(IntegerLiteralExprSyntax.self) else {
      throw DiagnosticError("expected integer literal, got '\(expr)'", node: expr)
    }
    guard let res = intLiteral.representedLiteralValue else {
      throw DiagnosticError("expected integer literal, got '\(expr)'", node: expr)
    }
    return res
  }

  static func getBoolLiteralValue(_ expr: ExprSyntax) throws -> Bool {
    guard let boolLiteral = expr.as(BooleanLiteralExprSyntax.self) else {
      throw DiagnosticError("expected boolean literal, got '\(expr)'", node: expr)
    }
    switch boolLiteral.literal.tokenKind {
    case .keyword(.true):
      return true
    case .keyword(.false):
      return false
    default:
      throw DiagnosticError("expected bool literal, got '\(expr)'", node: expr)
    }
  }

  static func parseCountedByEnum(
    _ enumConstructorExpr: FunctionCallExprSyntax, _ signature: FunctionSignatureSyntax
  ) throws -> ParamInfo {
    let argumentList = enumConstructorExpr.arguments
    let pointerParamIndexArg = try getArgumentByName(argumentList, "pointer")
    let pointerParamIndex: Int = try getIntLiteralValue(pointerParamIndexArg)
    let countExprArg = try getArgumentByName(argumentList, "count")
    guard let countExprStringLit = countExprArg.as(StringLiteralExprSyntax.self) else {
      throw DiagnosticError(
        "expected string literal for 'count' parameter, got \(countExprArg)", node: countExprArg)
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
    return CountedBy(
      pointerIndex: pointerParamIndex, count: unwrappedCountExpr, sizedBy: false,
      nonescaping: false, original: ExprSyntax(enumConstructorExpr))
  }

  static func parseSizedByEnum(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> ParamInfo {
    let argumentList = enumConstructorExpr.arguments
    let pointerParamIndexArg = try getArgumentByName(argumentList, "pointer")
    let pointerParamIndex: Int = try getIntLiteralValue(pointerParamIndexArg)
    let sizeExprArg = try getArgumentByName(argumentList, "size")
    guard let sizeExprStringLit = sizeExprArg.as(StringLiteralExprSyntax.self) else {
      throw DiagnosticError(
        "expected string literal for 'size' parameter, got \(sizeExprArg)", node: sizeExprArg)
    }
    let unwrappedCountExpr = ExprSyntax(stringLiteral: sizeExprStringLit.representedLiteralValue!)
    return CountedBy(
      pointerIndex: pointerParamIndex, count: unwrappedCountExpr, sizedBy: true, nonescaping: false,
      original: ExprSyntax(enumConstructorExpr))
  }

  static func parseEndedByEnum(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> ParamInfo {
    let argumentList = enumConstructorExpr.arguments
    let startParamIndexArg = try getArgumentByName(argumentList, "start")
    let startParamIndex: Int = try getIntLiteralValue(startParamIndexArg)
    let endParamIndexArg = try getArgumentByName(argumentList, "end")
    let endParamIndex: Int = try getIntLiteralValue(endParamIndexArg)
    let nonescapingExprArg = getOptionalArgumentByName(argumentList, "nonescaping")
    let nonescaping = try nonescapingExprArg != nil && getBoolLiteralValue(nonescapingExprArg!)
    return EndedBy(
      pointerIndex: startParamIndex, endIndex: endParamIndex, nonescaping: nonescaping,
      original: ExprSyntax(enumConstructorExpr))
  }

  static func parseNonEscaping(_ enumConstructorExpr: FunctionCallExprSyntax) throws -> Int {
    let argumentList = enumConstructorExpr.arguments
    let pointerParamIndexArg = try getArgumentByName(argumentList, "pointer")
    let pointerParamIndex: Int = try getIntLiteralValue(pointerParamIndexArg)
    return pointerParamIndex
  }

  static func parseTypeMappingParam(_ paramAST: LabeledExprSyntax?) throws -> [String: String]? {
    guard let unwrappedParamAST = paramAST else {
      return nil
    }
    let paramExpr = unwrappedParamAST.expression
    guard let dictExpr = paramExpr.as(DictionaryExprSyntax.self) else {
      return nil
    }
    var dict : [String: String] = [:]
    switch dictExpr.content {
      case .colon(_):
        return dict
      case .elements(let types):
        for element in types {
          guard let key = element.key.as(StringLiteralExprSyntax.self) else {
            throw DiagnosticError("expected a string literal, got '\(element.key)'", node: element.key)
          }
          guard let value = element.value.as(StringLiteralExprSyntax.self) else {
            throw DiagnosticError("expected a string literal, got '\(element.value)'", node: element.value)
          }
          dict[key.representedLiteralValue!] = value.representedLiteralValue!
        }
      default:
        throw DiagnosticError("unknown dictionary literal", node: dictExpr)
    }
    return dict
  }

  static func parseCxxSpanParams(
    _ signature: FunctionSignatureSyntax,
    _ typeMappings: [String: String]?
  ) throws -> [ParamInfo] {
    guard let typeMappings else {
      return []
    }
    var result : [ParamInfo] = []
    for (idx, param) in signature.parameterClause.parameters.enumerated() {
      let typeName = try getTypeName(param.type).text;
      if let desugaredType = typeMappings[typeName] {
        if desugaredType.starts(with: "span") {
          result.append(CxxSpan(pointerIndex: idx + 1, nonescaping: false,
            original: param, typeMappings: typeMappings))
        }
      }
    }
    return result
  }

  static func parseMacroParam(
    _ paramAST: LabeledExprSyntax, _ signature: FunctionSignatureSyntax,
    nonescapingPointers: inout Set<Int>
  ) throws -> ParamInfo? {
    let paramExpr = paramAST.expression
    guard let enumConstructorExpr = paramExpr.as(FunctionCallExprSyntax.self) else {
      throw DiagnosticError(
        "expected _SwiftifyInfo enum literal as argument, got '\(paramExpr)'", node: paramExpr)
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
    default:
      throw DiagnosticError(
        "expected 'countedBy', 'sizedBy', 'endedBy' or 'nonescaping', got '\(enumName)'",
        node: enumConstructorExpr)
    }
  }

  static func hasSafeVariants(_ parsedArgs: [ParamInfo]) -> Bool {
    return parsedArgs.contains { $0.nonescaping }
  }

  static func hasTrivialCountVariants(_ parsedArgs: [ParamInfo]) -> Bool {
    let countExprs = parsedArgs.compactMap {
      switch $0 {
      case let c as CountedBy: return c.count
      default: return nil
      }
    }
    let trivialCounts = countExprs.filter {
      $0.is(DeclReferenceExprSyntax.self) || $0.is(IntegerLiteralExprSyntax.self)
    }
    // don't generate trivial count variants if there are any non-trivial counts
    if trivialCounts.count < countExprs.count {
      return false
    }
    let countVars = trivialCounts.filter { $0.is(DeclReferenceExprSyntax.self) }
    let distinctCountVars = Set(
      countVars.map {
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
        let noteMessage =
          paramCount > 0
          ? "function \(funcDecl.name) has parameter indices 1..\(paramCount)"
          : "function \(funcDecl.name) has no parameters"
        throw DiagnosticError(
          "pointer index out of bounds", node: pointerArg.original,
          notes: [
            Note(node: Syntax(funcDecl.name), message: MacroExpansionNoteMessage(noteMessage))
          ])
      }
      if argByIndex[i] != nil {
        throw DiagnosticError(
          "multiple _SwiftifyInfos referring to parameter with index "
            + "\(i): \(pointerArg) and \(argByIndex[i]!)", node: pointerArg.original)
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
        throw DiagnosticError("@_SwiftifyImport only works on functions", node: declaration)
      }

      let argumentList = node.arguments!.as(LabeledExprListSyntax.self)!
      var arguments = Array<LabeledExprSyntax>(argumentList)
      let typeMappings = try parseTypeMappingParam(arguments.last)
      if typeMappings != nil {
        arguments = arguments.dropLast()
      }
      var nonescapingPointers = Set<Int>()
      var parsedArgs = try arguments.compactMap {
        try parseMacroParam($0, funcDecl.signature, nonescapingPointers: &nonescapingPointers)
      }
      parsedArgs.append(contentsOf: try parseCxxSpanParams(funcDecl.signature, typeMappings))
      setNonescapingPointers(&parsedArgs, nonescapingPointers)
      parsedArgs = parsedArgs.filter {
        !($0 is CxxSpan) || ($0 as! CxxSpan).nonescaping
      }
      try checkArgs(parsedArgs, funcDecl)
      let baseBuilder = FunctionCallBuilder(funcDecl)

      let variant = Variant(
        generateSpan: hasSafeVariants(parsedArgs),
        skipTrivialCount: hasTrivialCountVariants(parsedArgs))

      let builder: BoundsCheckedThunkBuilder = parsedArgs.reduce(
        baseBuilder,
        { (prev, parsedArg) in
          parsedArg.getBoundsCheckedThunkBuilder(prev, funcDecl, variant)
        })
      let newSignature = try builder.buildFunctionSignature([:], variant)
      let checks =
        variant.skipTrivialCount
        ? [] as [CodeBlockItemSyntax]
        : try builder.buildBoundsChecks(variant).map { e in
          CodeBlockItemSyntax(leadingTrivia: "\n", item: e)
        }
      let call = CodeBlockItemSyntax(
        item: CodeBlockItemSyntax.Item(
          ReturnStmtSyntax(
            returnKeyword: .keyword(.return, trailingTrivia: " "),
            expression: try builder.buildFunctionCall([:], variant))))
      let body = CodeBlockSyntax(statements: CodeBlockItemListSyntax(checks + [call]))
      let newFunc =
        funcDecl
        .with(\.signature, newSignature)
        .with(\.body, body)
        .with(
          \.attributes,
          funcDecl.attributes.filter { e in
            switch e {
            case .attribute(let attr):
              // don't apply this macro recursively, and avoid dupe _alwaysEmitIntoClient
              let name = attr.attributeName.as(IdentifierTypeSyntax.self)?.name.text
              return name == nil || (name != "_SwiftifyImport" && name != "_alwaysEmitIntoClient")
            default: return true
            }
          } + [
            .attribute(
              AttributeSyntax(
                atSign: .atSignToken(),
                attributeName: IdentifierTypeSyntax(name: "_alwaysEmitIntoClient")))
          ])
      return [DeclSyntax(newFunc)]
    } catch let error as DiagnosticError {
      context.diagnose(
        Diagnostic(
          node: error.node, message: MacroExpansionErrorMessage(error.description),
          notes: error.notes))
      return []
    }
  }
}

// MARK: syntax utils
extension TypeSyntaxProtocol {
  public var isSwiftCoreModule: Bool {
    guard let identifierType = self.as(IdentifierTypeSyntax.self) else {
      return false
    }
    return identifierType.name.text == "Swift"
  }

  /// Check if this syntax could resolve to the type passed. Only supports types where the canonical type
  /// can be named using only IdentifierTypeSyntax and MemberTypeSyntax. A non-exhaustive list of unsupported
  /// types includes:
  /// * array types
  /// * function types
  /// * optional types
  /// * tuple types (including Void!)
  /// The type syntax is allowed to use any level of qualified name for the type, e.g. Swift.Int.self
  /// will match against both "Swift.Int" and "Int".
  ///
  /// - Parameter type: Type to check against. NB: if passing a type alias, the canonical type will be used.
  /// - Returns: true if `self` spells out some suffix of the fully qualified name of `type`, otherwise false
  public func canRepresentBasicType(type: Any.Type) -> Bool {
    let qualifiedTypeName = String(reflecting: type)
    var typeNames = qualifiedTypeName.split(separator: ".")
    var currType: TypeSyntaxProtocol = self

    while !typeNames.isEmpty {
      let typeName = typeNames.popLast()!
      if let identifierType = currType.as(IdentifierTypeSyntax.self) {
        // It doesn't matter whether this is the final element of typeNames, because we don't know
        // surrounding context - the Foo.Bar.Baz type can be referred to as `Baz` inside Foo.Bar
        return identifierType.name.text == typeName
      } else if let memberType = currType.as(MemberTypeSyntax.self) {
        if memberType.name.text != typeName {
          return false
        }
        currType = memberType.baseType
      } else {
        return false
      }
    }

    return false
  }
}
