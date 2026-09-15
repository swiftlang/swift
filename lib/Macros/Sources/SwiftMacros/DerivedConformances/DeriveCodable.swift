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

private let codingKeysRef = "Self.CodingKeys"

private enum CaseDecoding {
  case unavailable
  case nested(keysRef: String)
}

extension CodableTypeInfo {
  fileprivate var unsafeMark: String { isUnsafe ? "unsafe " : "" }
}

extension CodedCase {
  fileprivate var caseCodingKeysRef: String? {
    caseCodingKeysName.map { "Self.\($0)" }
  }

  fileprivate var encodingKeys: (keyName: String, keysRef: String)? {
    guard !isUnavailable, let keyName, let caseCodingKeysRef else {
      return nil
    }
    return (keyName, caseCodingKeysRef)
  }

  fileprivate var isEncodable: Bool { encodingKeys != nil }

  fileprivate var decoding: (keyName: String, decoding: CaseDecoding)? {
    guard let keyName else { return nil }
    if isUnavailable {
      return (keyName, .unavailable)
    }
    guard let caseCodingKeysRef else { return nil }
    return (keyName, .nested(keysRef: caseCodingKeysRef))
  }

  /// The payload values carrying a key, paired with their binding index.
  fileprivate var encodedPayload: [(index: Int, keyName: String, useIfPresent: Bool)] {
    payload.indices.compactMap {
      index -> (index: Int, keyName: String, useIfPresent: Bool)? in
      guard let keyName = payload[index].keyName else { return nil }
      return (index, keyName, payload[index].useIfPresent)
    }
  }

  fileprivate var decodedPayload:
    [(keyName: String, label: String?, typeName: String, useIfPresent: Bool)]
  {
    payload.compactMap {
      value -> (keyName: String, label: String?, typeName: String, useIfPresent: Bool)? in
      guard let keyName = value.keyName else { return nil }
      return (keyName, value.label, value.typeName, value.useIfPresent)
    }
  }

  fileprivate var encodePattern: PatternSyntax {
    guard isEncodable, !encodedPayload.isEmpty else {
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

  let info: CodableTypeInfo

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let info = try node.arguments.expect(
      .init(parser: CodableTypeInfo.fromStringLit))

    return [Self(info: info).encodeDecl]
  }

  var encodeDecl: DeclSyntax {
    """
    func encode(to encoder: any Swift::Encoder) throws {
      \(body)
    }
    """
  }

  var body: CodeBlockItemListSyntax {
    switch info.shape {
    case .structLike(let properties):
      structBody(properties)
    case .enumLike(let cases):
      enumBody(cases)
    }
  }

  func structBody(_ properties: [CodedProperty]) -> CodeBlockItemListSyntax {
    if properties.isEmpty {
      return """
        _ = encoder.container(keyedBy: \(raw: codingKeysRef).self)
        """
    }

    var items: [CodeBlockItemSyntax] = [
      """
      var container = encoder.container(keyedBy: \(raw: codingKeysRef).self)
      """
    ]

    for property in properties {
      let method = property.useIfPresent ? "encodeIfPresent" : "encode"
      items.append(
        """
        try \(raw: info.unsafeMark)container.\(raw: method)(self.\(raw: property.memberName), forKey: .\(raw: property.keyName))
        """
      )
    }

    return .init(items)
  }

  func enumBody(_ cases: [CodedCase]) -> CodeBlockItemListSyntax {
    let containerCall: ExprSyntax =
      "encoder.container(keyedBy: \(raw: codingKeysRef).self)"

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

    let caseSyntax = cases.map(encodeCase)

    return """
      \(containerDecl)
      switch \(raw: info.unsafeMark)self {
      \(raw: caseSyntax.map { $0.trimmedDescription }.joined(separator: "\n"))
      }
      """
  }

  func encodeCase(_ codedCase: CodedCase) -> SwitchCaseSyntax {
    var items: [CodeBlockItemSyntax] = []

    if codedCase.isUnavailable {
      items.append(getUnreachableStatement())
    } else if let (keyName, keysRef) = codedCase.encodingKeys {
      let encoded = codedCase.encodedPayload

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
        for (index, valueKeyName, useIfPresent) in encoded {
          let method = useIfPresent ? "encodeIfPresent" : "encode"
          items.append(
            """
            try \(raw: info.unsafeMark)nestedContainer.\(raw: method)(a\(raw: index), forKey: .\(raw: valueKeyName))
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

  let info: CodableTypeInfo

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let info = try node.arguments.expect(
      .init(parser: CodableTypeInfo.fromStringLit))

    return [Self(info: info).initDecl]
  }

  var initDecl: DeclSyntax {
    """
    init(from decoder: any Swift::Decoder) throws {
      \(body)
    }
    """
  }

  var body: CodeBlockItemListSyntax {
    guard info.hasCodingKeys else {
      return ""
    }

    switch info.shape {
    case .structLike(let properties):
      return structBody(properties)
    case .enumLike(let cases):
      return enumBody(cases)
    }
  }

  func structBody(_ properties: [CodedProperty]) -> CodeBlockItemListSyntax {
    if properties.isEmpty {
      return """
        _ = try decoder.container(keyedBy: \(raw: codingKeysRef).self)
        """
    }

    var items: [CodeBlockItemSyntax] = [
      """
      let container = try decoder.container(keyedBy: \(raw: codingKeysRef).self)
      """
    ]

    for property in properties {
      let method = property.useIfPresent ? "decodeIfPresent" : "decode"
      items.append(
        """
        \(raw: info.unsafeMark)self.\(raw: property.memberName) = try container.\(raw: method)(\(raw: property.typeName).self, forKey: .\(raw: property.keyName))
        """
      )
    }

    return .init(items)
  }

  func enumBody(_ cases: [CodedCase]) -> CodeBlockItemListSyntax {
    var items: [CodeBlockItemSyntax] = []

    items.append(
      """
      let container = try decoder.container(keyedBy: \(raw: codingKeysRef).self)
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

    let caseSyntax = cases.compactMap(decodeCase)
    items.append(
      """
      switch onlyKey {
      \(raw: caseSyntax.map { $0.trimmedDescription }.joined(separator: "\n"))
      }
      """
    )

    return .init(items)
  }

  func decodeCase(_ codedCase: CodedCase) -> SwitchCaseSyntax? {
    guard let (keyName, decoding) = codedCase.decoding else {
      return nil
    }

    var items: [CodeBlockItemSyntax] = []

    switch decoding {
    case .unavailable:
      items.append(
        """
        throw Swift::DecodingError.dataCorrupted(Swift::DecodingError.Context(codingPath: container.codingPath, debugDescription: "Unavailable enum element encountered.", underlyingError: nil))
        """
      )
    case .nested(let keysRef):
      let decoded = codedCase.decodedPayload

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
          \(raw: info.unsafeMark)self = .\(raw: codedCase.name)\(raw: parens)
          """
        )
      } else {
        items.append(
          """
          \(raw: info.unsafeMark)self = .\(raw: codedCase.name)(\(raw: args.joined(separator: ", ")))
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
