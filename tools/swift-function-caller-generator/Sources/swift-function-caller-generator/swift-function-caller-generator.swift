import Foundation
import SwiftParser
import SwiftSyntax
import SwiftSyntaxMacros

@main
class SwiftMacroTestGen: SyntaxVisitor {
  static func main() {
    if CommandLine.argc < 2 {
      print("error: missing module name (passed 1 argument, expected 2)")
      exit(1)
    }
    let contents =
      if CommandLine.argc > 2 {
        read(file: CommandLine.arguments[2])
      } else {
        readStdin()
      }
    let syntaxTree = Parser.parse(source: contents)
    print("import \(CommandLine.arguments[1])\n")
    let visitor = SwiftMacroTestGen(viewMode: .all)
    visitor.walk(syntaxTree)
  }

  var typeAlias: [String: TypeSyntax] = [:]
  override func visit(_ node: TypeAliasDeclSyntax) -> SyntaxVisitorContinueKind {
    let typeAliasName = node.name.trimmedDescription
    let rhsType = node.initializer.value
    typeAlias[typeAliasName] = rhsType
    return .skipChildren
  }

  override func visit(_ node: FunctionDeclSyntax) -> SyntaxVisitorContinueKind {
    var res = node
    if res.attributes.contains(where: { $0.isObsolete }) {
      // don't try to call the old name of a renamed function
      return .skipChildren
    }
    let surroundingType = getParentType(res)
    let selfParam = surroundingType.map { _ in TokenSyntax("self") }
    res = createFunctionSignature(res)
    res =
      res
      .with(\.body, createBody(res, selfParam: selfParam))
      .with(\.name, "call_\(res.name.withoutBackticks)")
    if let surroundingType {
      res =
        res
        .with(
          \.signature.parameterClause.parameters,
          addSelfParam(
            res.signature.parameterClause.parameters, surroundingType, selfParam!)
        )
        .with(\.leadingTrivia, "\n")
    }
    print(res)
    return .skipChildren
  }

  func createFunctionSignature(_ f: FunctionDeclSyntax) -> FunctionDeclSyntax {
    let params = f.signature.parameterClause.parameters
    let funcName = f.name.withoutBackticks.trimmed.text
    let newParams = params.enumerated().map { (i, param: FunctionParameterSyntax) in
      let paramName = param.name.trimmed.text
      var newParam = param
      if paramName == "_" || paramName == funcName || "`\(paramName)`" == funcName {
        let secondName = TokenSyntax("_\(raw: funcName)_param\(raw: i)").with(
          \.leadingTrivia, " ")
        let firstName = newParam.firstName
        newParam = newParam.with(\.secondName, secondName)
          .with(\.firstName, firstName)
      }
      // compiler warns if "var" or "let" are used as parameter labels unescaped
      if newParam.firstName.trimmedDescription == "var"
        || newParam.firstName.trimmedDescription == "let"
      {
        let firstName = newParam.firstName.escaped
        newParam = newParam.with(\.firstName, firstName)
      }
      // replace type aliases with the concrete type so that `hasUnsafeType` can inspect
      // whether we need to add `unsafe`
      newParam = newParam.with(\.type, TypeAliasReplacer(typeAlias).visit(newParam.type))
      return newParam
    }
    return f.with(
      \.signature.parameterClause.parameters, FunctionParameterListSyntax(newParams))
  }
}

class TypeAliasReplacer: SyntaxRewriter {
  let typeAlias: [String: TypeSyntax]
  init(_ typeAlias: [String: TypeSyntax]) {
    self.typeAlias = typeAlias
  }
  override func visit(_ node: IdentifierTypeSyntax) -> TypeSyntax {
    if let newType = typeAlias[node.name.trimmedDescription] {
      return newType
    }
    return TypeSyntax(node)
  }
}

func read(file path: String) -> String {
  do {
    return try String(contentsOfFile: path, encoding: .utf8)
  } catch {
    print("Error reading file \(path): \(error.localizedDescription)")
    exit(1)
  }
}

func readStdin() -> String {
  if let data = try? FileHandle.standardInput.readToEnd(),
    let input = String(data: data, encoding: .utf8)
  {
    return input
  } else {
    print("Error reading stdin)")
    exit(1)
  }
}

func createBody(_ f: FunctionDeclSyntax, selfParam: TokenSyntax?) -> CodeBlockSyntax {
  var call = createCall(f)
  if let selfParam {
    call = "\(selfParam).\(call)"
  }
  return
    """
     {
      return \(call)
    }
    """
}

func createCall(_ f: FunctionDeclSyntax) -> ExprSyntax {
  let args = f.signature.parameterClause.parameters.map { param in
    var declRef = ExprSyntax(DeclReferenceExprSyntax(baseName: param.name.escapeIfNeeded))
    if param.type.isInout {
      declRef = "&\(declRef)"
    }
    return declRef
  }
  let labels: [TokenSyntax?] = f.signature.parameterClause.parameters.map { param in
    let firstName = param.firstName.trimmed
    if firstName.text == "_" {
      return nil
    }
    return firstName
  }
  let labeledArgs: [LabeledExprSyntax] = zip(labels, args).enumerated().map { (i, e) in
    let (label, arg) = e
    let comma: TokenSyntax? = i < args.count - 1 ? .commaToken(trailingTrivia: " ") : nil
    let colon: TokenSyntax? = label != nil ? .colonToken(trailingTrivia: " ") : nil
    return LabeledExprSyntax(
      label: label?.withoutBackticks, colon: colon, expression: arg, trailingComma: comma)
  }
  let unsafeKw = hasUnsafeType(f) ? "unsafe " : ""
  return ExprSyntax("\(raw: unsafeKw)\(f.name)(\(LabeledExprListSyntax(labeledArgs)))")
}

func hasUnsafeType(_ f: FunctionDeclSyntax) -> Bool {
  if f.signature.returnClause?.type.isUnsafe ?? false {
    return true
  }
  return f.signature.parameterClause.parameters.contains(where: { $0.type.isUnsafe })
}

extension TypeSyntax {
  var isUnsafe: Bool {
    if self.description.contains("Unsafe") {
      return true
    }
    if self.description.contains("OpaquePointer") {
      return true
    }
    return false
  }

  var isInout: Bool {
    guard let attr = self.as(AttributedTypeSyntax.self) else {
      return false
    }
    return attr.specifiers.contains(where: { e in
      guard let simpleSpec = e.as(SimpleTypeSpecifierSyntax.self) else {
        return false
      }
      return simpleSpec.specifier.text == "inout"
    })
  }
}

func addSelfParam(_ params: FunctionParameterListSyntax, _ type: TokenSyntax, _ name: TokenSyntax)
  -> FunctionParameterListSyntax
{
  return [FunctionParameterSyntax("_ \(name): \(type.trimmed), ")] + params
}

func getParentType(_ node: some SyntaxProtocol) -> TokenSyntax? {
  guard let parent = node.parent else {
    return nil
  }
  if let structType = parent.as(StructDeclSyntax.self) {
    return structType.name
  }
  if let classType = parent.as(ClassDeclSyntax.self) {
    return classType.name
  }
  return getParentType(parent)
}

extension FunctionParameterSyntax {
  var name: TokenSyntax {
    self.secondName ?? self.firstName
  }
}

enum TokenEscapeContext {
  case declRef
  case label
}

extension TokenSyntax {
  var withoutBackticks: TokenSyntax {
    if self.identifier == nil {
      return self
    }
    return .identifier(self.identifier!.name)
  }
  var escaped: TokenSyntax {
    return self.copyTrivia(to: "`\(raw: self.trimmed.text)`")
  }
  var escapeIfNeeded: TokenSyntax {
    var parser = Parser("let \(self)")
    let decl = DeclSyntax.parse(from: &parser)
    if !decl.hasError {
      return self
    } else {
      return self.escaped
    }
  }

  func copyTrivia(to other: TokenSyntax) -> TokenSyntax {
    return .identifier(
      other.text, leadingTrivia: self.leadingTrivia, trailingTrivia: self.trailingTrivia)
  }
}

extension Optional {
  var asList: [Wrapped] {
    if let self {
      return [self]
    } else {
      return []
    }
  }
}

extension AttributeSyntax {
  var isObsolete: Bool {
    guard self.attributeName.trimmed.description == "available" else {
      return false
    }
    guard let args = self.arguments else {
      return false
    }
    return switch args {
    case .availability(let list):
      list.contains(where: {
        $0.argument.as(AvailabilityLabeledArgumentSyntax.self)?.label.trimmed.description
          == "obsoleted"
      })
    default: false
    }
  }
}
extension AttributeListSyntax.Element {
  var isObsolete: Bool {
    switch self {
    case .attribute(let a): return a.isObsolete
    case .ifConfigDecl: return false
    }
  }
}
