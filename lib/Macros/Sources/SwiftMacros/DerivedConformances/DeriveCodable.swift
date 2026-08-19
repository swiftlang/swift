//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct CodedProperty {
  var keyName: String
  var memberName: String
  var typeName: String
  var useIfPresent: Bool
}

public struct CodedPayload {
  var label: String?
  var keyName: String?
  var typeName: String
  var useIfPresent: Bool
}

public struct CodedCase {
  var name: String
  var caseCodingKeysName: String?
  var keyName: String?
  var isUnavailable: Bool
  var payload: [CodedPayload]
}

public enum CodableShape {
  case structLike([CodedProperty])
  case enumLike([CodedCase])
}

public struct CodableTypeInfo {
  var isUnsafe: Bool
  var hasCodingKeys: Bool
  var shape: CodableShape
}

extension CodedProperty: TypeInfoProtocol {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    let (keyName, memberName, typeName, useIfPresent) = try getNamedFuncallArgs(
      node: node,
      name: "CodedProperty"
    ).expect(
      .stringArg("keyName"),
      .stringArg("memberName"),
      .stringArg("typeName"),
      .boolArg("useIfPresent")
    )

    return Self(
      keyName: keyName, memberName: memberName, typeName: typeName,
      useIfPresent: useIfPresent)
  }

  public var syntax: ExprSyntax {
    """
    CodedProperty(keyName: \(stringlit(keyName)), memberName: \(stringlit(memberName)), typeName: \(stringlit(typeName)), useIfPresent: \(boollit(useIfPresent)))
    """
  }
}

extension CodedPayload: TypeInfoProtocol {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    // Expecting:
    //   CodedPayload(label: <String?>,
    //                keyName: <String?>,
    //                typeName: <String>,
    //                useIfPresent: <Bool>)

    let (label, keyName, typeName, useIfPresent) = try getNamedFuncallArgs(
      node: node,
      name: "CodedPayload"
    ).expect(
      .stringArg("label").toOptional(),
      .stringArg("keyName").toOptional(),
      .stringArg("typeName"),
      .boolArg("useIfPresent")
    )

    return Self(
      label: label, keyName: keyName, typeName: typeName,
      useIfPresent: useIfPresent)
  }

  public var syntax: ExprSyntax {
    """
    CodedPayload(label: \(optionalSyntax(label, stringlit)), keyName: \(optionalSyntax(keyName, stringlit)), typeName: \(stringlit(typeName)), useIfPresent: \(boollit(useIfPresent)))
    """
  }
}

extension CodedCase: TypeInfoProtocol {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    // Expecting:
    //   CodedCase(name: <String>,
    //             caseCodingKeysName: <String?>,
    //             keyName: <String?>,
    //             isUnavailable: <Bool>,
    //             payload: <[CodedPayload]>)

    let (name, caseCodingKeysName, keyName, isUnavailable, payload) =
      try getNamedFuncallArgs(node: node, name: "CodedCase").expect(
        .stringArg("name"),
        .stringArg("caseCodingKeysName").toOptional(),
        .stringArg("keyName").toOptional(),
        .boolArg("isUnavailable"),
        .arrayArg("payload", parser: CodedPayload.fromSyntax)
      )

    return Self(
      name: name,
      caseCodingKeysName: caseCodingKeysName,
      keyName: keyName,
      isUnavailable: isUnavailable,
      payload: payload
    )
  }

  public var syntax: ExprSyntax {
    """
    CodedCase(name: \(stringlit(name)), caseCodingKeysName: \(optionalSyntax(caseCodingKeysName, stringlit)), keyName: \(optionalSyntax(keyName, stringlit)), isUnavailable: \(boollit(isUnavailable)), payload: \(arraySyntax(payload)))
    """
  }
}

extension CodableShape: TypeInfoProtocol {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    // Expecting:
    //   structLike(<[CodedProperty]>)
    // or
    //   enumLike(<[CodedCase]>)

    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw TypeInfoParseError.expectedFunctionCall(got: node)
    }
    switch fcall.calledExpression.trimmedDescription {
    case "structLike":
      return try .structLike(
        fcall.arguments.expect(
          .arrayArg(nil, parser: CodedProperty.fromSyntax)))
    case "enumLike":
      return try .enumLike(
        fcall.arguments.expect(.arrayArg(nil, parser: CodedCase.fromSyntax)))
    default:
      throw TypeInfoParseError.expectedFunctionCallNames(
        names: ["structLike", "enumLike"],
        got: fcall.calledExpression
      )
    }
  }

  public var syntax: ExprSyntax {
    switch self {
    case .structLike(let properties):
      """
      structLike(\(arraySyntax(properties)))
      """
    case .enumLike(let cases):
      """
      enumLike(\(arraySyntax(cases)))
      """
    }
  }
}

extension CodableTypeInfo: TypeInfoProtocol {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    // Expecting:
    //   CodableTypeInfo(isUnsafe: <Bool>,
    //                   hasCodingKeys: <Bool>,
    //                   shape: <CodableShape>)

    let (isUnsafe, hasCodingKeys, shape) = try getNamedFuncallArgs(
      node: node,
      name: "CodableTypeInfo"
    ).expect(
      .boolArg("isUnsafe"),
      .boolArg("hasCodingKeys"),
      .init(name: "shape", parser: CodableShape.fromSyntax)
    )

    return Self(isUnsafe: isUnsafe, hasCodingKeys: hasCodingKeys, shape: shape)
  }

  public var syntax: ExprSyntax {
    """
    CodableTypeInfo(isUnsafe: \(boollit(isUnsafe)),
                    hasCodingKeys: \(boollit(hasCodingKeys)),
                    shape: \(shape.syntax))
    """
  }
}

private func unsafeMark(_ isUnsafe: Bool) -> String {
  isUnsafe ? "unsafe " : ""
}

private func codingKeysRef(_ name: String) -> String {
  "Self.\(name)"
}

extension CodedCase {
  fileprivate var encodingKeys: (keyName: String, caseCodingKeysName: String)? {
    guard !isUnavailable, let keyName, let caseCodingKeysName else {
      return nil
    }
    return (keyName, caseCodingKeysName)
  }

  fileprivate var isEncodable: Bool { encodingKeys != nil }

  fileprivate var encodePattern: PatternSyntax {
    let encodesAnyValue = payload.contains { $0.keyName != nil }
    if payload.isEmpty || !isEncodable || !encodesAnyValue {
      return ".\(raw: name)"
    }

    let bindings = payload.enumerated().map { i, value in
      let label = value.label.map { "\($0): " } ?? ""
      return value.keyName == nil ? "\(label)_" : "\(label)let a\(i)"
    }

    return ".\(raw: name)(\(raw: bindings.joined(separator: ", ")))"
  }
}

public struct DeriveEncodableMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let info = try node.arguments.expect(
      .init(parser: CodableTypeInfo.fromStringLit))

    return [Self.deriveEncode(info)]
  }

  static func deriveEncode(_ info: CodableTypeInfo) -> DeclSyntax {
    """
    func encode(to encoder: any Swift::Encoder) throws {
      \(getBody(info))
    }
    """
  }

  static func getBody(_ info: CodableTypeInfo) -> CodeBlockItemListSyntax {
    switch info.shape {
    case .structLike(let properties):
      getStructBody(properties, isUnsafe: info.isUnsafe)
    case .enumLike(let cases):
      getEnumBody(cases, info: info)
    }
  }

  static func getStructBody(
    _ properties: [CodedProperty], isUnsafe: Bool
  ) -> CodeBlockItemListSyntax {
    if properties.isEmpty {
      return """
        _ = encoder.container(keyedBy: \(raw: codingKeysRef("CodingKeys")).self)
        """
    }

    let mark = unsafeMark(isUnsafe)
    var items: [CodeBlockItemSyntax] = [
      """
      var container = encoder.container(keyedBy: \(raw: codingKeysRef("CodingKeys")).self)
      """
    ]

    for property in properties {
      let method = property.useIfPresent ? "encodeIfPresent" : "encode"
      items.append(
        """
        try \(raw: mark)container.\(raw: method)(self.\(raw: property.memberName), forKey: .\(raw: property.keyName))
        """
      )
    }

    return .init(items)
  }

  static func getEnumBody(
    _ cases: [CodedCase], info: CodableTypeInfo
  ) -> CodeBlockItemListSyntax {
    let containerCall: ExprSyntax =
      "encoder.container(keyedBy: \(raw: codingKeysRef("CodingKeys")).self)"

    if cases.isEmpty {
      return """
        _ = \(containerCall)
        switch self {}
        """
    }

    let mutatesContainer = cases.contains { $0.isEncodable }
    let readsContainer = cases.contains { !$0.isUnavailable && !$0.isEncodable }

    let containerDecl: CodeBlockItemSyntax =
      if mutatesContainer {
        "var container = \(containerCall)"
      } else if readsContainer {
        "let container = \(containerCall)"
      } else {
        "_ = \(containerCall)"
      }

    let caseSyntax = cases.map { getCase($0, isUnsafe: info.isUnsafe) }

    return """
      \(containerDecl)
      switch self {
      \(raw: caseSyntax.map { $0.trimmedDescription }.joined(separator: "\n"))
      }
      """
  }

  static func getCase(
    _ codedCase: CodedCase, isUnsafe: Bool
  ) -> SwitchCaseSyntax {
    var items: [CodeBlockItemSyntax] = []

    if codedCase.isUnavailable {
      items.append(getUnreachableStatement())
    } else if let (keyName, caseCodingKeysName) = codedCase.encodingKeys {
      let keysRef = codingKeysRef(caseCodingKeysName)
      let encoded = codedCase.payload.enumerated().compactMap {
        (i, value) in value.keyName.map { (i, $0, value.useIfPresent) }
      }

      if encoded.isEmpty {
        items.append(
          """
          _ = container.nestedContainer(keyedBy: \(raw: keysRef).self, forKey: .\(raw: keyName))
          """
        )
      } else {
        items.append(
          """
          var nestedContainer = container.nestedContainer(keyedBy: \(raw: keysRef).self, forKey: .\(raw: keyName))
          """
        )
        let mark = unsafeMark(isUnsafe)
        for (i, valueKeyName, useIfPresent) in encoded {
          let method = useIfPresent ? "encodeIfPresent" : "encode"
          items.append(
            """
            try \(raw: mark)nestedContainer.\(raw: method)(a\(raw: i), forKey: .\(raw: valueKeyName))
            """
          )
        }
      }
    } else {
      // The case is not defined in CodingKeys, so it cannot be encoded.
      let message =
        "Case '\(codedCase.name)' cannot be encoded because it is not defined in CodingKeys."
      items.append(
        """
        throw Swift::EncodingError.invalidValue(self, Swift::EncodingError.Context(codingPath: container.codingPath, debugDescription: \(stringlit(message)), underlyingError: nil))
        """
      )
    }

    return """
      case \(codedCase.encodePattern):
        \(CodeBlockItemListSyntax(items))
      """
  }
}

public struct DeriveDecodableMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let info = try node.arguments.expect(
      .init(parser: CodableTypeInfo.fromStringLit))

    return [Self.deriveInit(info)]
  }

  static func deriveInit(_ info: CodableTypeInfo) -> DeclSyntax {
    """
    init(from decoder: any Swift::Decoder) throws {
      \(getBody(info))
    }
    """
  }

  static func getBody(_ info: CodableTypeInfo) -> CodeBlockItemListSyntax {
    switch info.shape {
    case .structLike(let properties):
      getStructBody(properties, info: info)
    case .enumLike(let cases):
      getEnumBody(cases, info: info)
    }
  }

  static func getStructBody(
    _ properties: [CodedProperty], info: CodableTypeInfo
  ) -> CodeBlockItemListSyntax {
    if !info.hasCodingKeys {
      return ""
    }

    if properties.isEmpty {
      return """
        _ = try decoder.container(keyedBy: \(raw: codingKeysRef("CodingKeys")).self)
        """
    }

    var items: [CodeBlockItemSyntax] = [
      """
      let container = try decoder.container(keyedBy: \(raw: codingKeysRef("CodingKeys")).self)
      """
    ]

    let mark = unsafeMark(info.isUnsafe)
    for property in properties {
      let method = property.useIfPresent ? "decodeIfPresent" : "decode"
      items.append(
        """
        \(raw: mark)self.\(raw: property.memberName) = try container.\(raw: method)(\(raw: property.typeName).self, forKey: .\(raw: property.keyName))
        """
      )
    }

    return .init(items)
  }

  static func getEnumBody(
    _ cases: [CodedCase], info: CodableTypeInfo
  ) -> CodeBlockItemListSyntax {
    if !info.hasCodingKeys {
      return ""
    }

    let decodable = cases.compactMap { c -> (CodedCase, String, String)? in
      guard let keyName = c.keyName else { return nil }
      guard let caseKeys = c.caseCodingKeysName else {
        return c.isUnavailable ? (c, keyName, "") : nil
      }
      return (c, keyName, caseKeys)
    }

    var items: [CodeBlockItemSyntax] = []

    items.append(
      """
      let container = try decoder.container(keyedBy: \(raw: codingKeysRef("CodingKeys")).self)
      """
    )
    items.append(
      """
      var allKeys = Swift::ArraySlice(container.allKeys)
      """
    )
    items.append(
      """
      guard let onlyKey = allKeys.popFirst(), allKeys.isEmpty else {
        throw Swift::DecodingError.typeMismatch(Self.self, Swift::DecodingError.Context(codingPath: container.codingPath, debugDescription: "Invalid number of keys found, expected one.", underlyingError: nil))
      }
      """
    )

    let caseSyntax = decodable.map { c, keyName, caseCodingKeysName in
      getCase(
        c, keyName: keyName, caseCodingKeysName: caseCodingKeysName,
        isUnsafe: info.isUnsafe)
    }
    items.append(
      """
      switch onlyKey {
      \(raw: caseSyntax.map { $0.trimmedDescription }.joined(separator: "\n"))
      }
      """
    )

    return .init(items)
  }

  static func getCase(
    _ codedCase: CodedCase, keyName: String, caseCodingKeysName: String,
    isUnsafe: Bool
  ) -> SwitchCaseSyntax {
    let mark = unsafeMark(isUnsafe)
    var items: [CodeBlockItemSyntax] = []

    if codedCase.isUnavailable {
      items.append(
        """
        throw Swift::DecodingError.dataCorrupted(Swift::DecodingError.Context(codingPath: container.codingPath, debugDescription: "Unavailable enum element encountered.", underlyingError: nil))
        """
      )
    } else {
      let keysRef = codingKeysRef(caseCodingKeysName)
      let decoded = codedCase.payload.compactMap {
        value in
        value.keyName.map { ($0, value.label, value.typeName, value.useIfPresent) }
      }

      if decoded.isEmpty {
        items.append(
          """
          _ = try container.nestedContainer(keyedBy: \(raw: keysRef).self, forKey: .\(raw: keyName))
          """
        )
      } else {
        items.append(
          """
          let nestedContainer = try container.nestedContainer(keyedBy: \(raw: keysRef).self, forKey: .\(raw: keyName))
          """
        )
      }

      let args = decoded.map {
        valueKeyName, label, typeName, useIfPresent -> String in
        let labelText = label.map { "\($0): " } ?? ""
        let method = useIfPresent ? "decodeIfPresent" : "decode"
        return
          "\(labelText)try nestedContainer.\(method)(\(typeName).self, forKey: .\(valueKeyName))"
      }

      if args.isEmpty {
        let parens = codedCase.payload.isEmpty ? "" : "()"
        items.append(
          """
          \(raw: mark)self = .\(raw: codedCase.name)\(raw: parens)
          """
        )
      } else {
        items.append(
          """
          \(raw: mark)self = .\(raw: codedCase.name)(\(raw: args.joined(separator: ", ")))
          """
        )
      }
    }

    return """
      case .\(raw: keyName):
        \(CodeBlockItemListSyntax(items))
      """
  }
}
