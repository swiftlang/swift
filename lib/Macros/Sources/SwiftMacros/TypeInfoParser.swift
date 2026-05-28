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
// Utilities to handle type information passed as arguments to macros         //
// conformances.                                                              //
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxBuilder

public struct NominalTypeInfo {
  var name: String
  var kind: NominalTypeKind
  var isUnsafe: Bool
}

public enum NominalTypeKind {
  case enumLike(EnumTypeInfo)
  case structLike(StructTypeInfo)
}

public struct EnumTypeInfo {
  var isObjC: Bool
  var cases: [CaseInfo]
}

public struct CaseInfo {
  var name: String
  var associatedValues: [String?]
}

public struct StructTypeInfo {
  var properties: [StoredProperty]
}

public struct StoredProperty {
  var name: String
  var typeName: String
  var isVar: Bool
  var isStatic: Bool
}

public enum TypeInfoParseError: Error {
  case badArgName(expected: String?, got: LabeledExprSyntax)
  case argCountMismatch(expected: Int, args: LabeledExprListSyntax)
  case expectedStringLiteral(got: ExprSyntax)
  case expectedBoolLiteral(got: ExprSyntax)
  case expectedArrayLiteral(got: ExprSyntax)
  case expectedFunctionCall(got: ExprSyntax)
  case expectedFunctionCallNames(names: [String], got: ExprSyntax)

  case expectedStrLitAsInput(got: ExprSyntax)
}

func parseString(node: ExprSyntax) throws -> String {
  guard let res = node.as(StringLiteralExprSyntax.self)?.representedLiteralValue else {
    throw TypeInfoParseError.expectedStringLiteral(got: node)
  }
  return res
}

func parseBool(node: ExprSyntax) throws -> Bool {
  guard let lit = node.as(BooleanLiteralExprSyntax.self) else {
    throw TypeInfoParseError.expectedBoolLiteral(got: node)
  }
  return lit.trimmedDescription == "true"
}

func parseOpt<T>(node: ExprSyntax, parser f: (ExprSyntax) throws -> T) throws -> T? {
  // Eagerly checks if it is nil. This means that T?? can never be some(nil)
  // parsed this way as we just expect either nil or a value.
  if node.is(NilLiteralExprSyntax.self) {
    return nil
  }
  return try f(node)
}

func parseArr<T>(node: ExprSyntax, parser f: (ExprSyntax) throws -> T) throws -> [T] {
  guard let arr = node.as(ArrayExprSyntax.self) else {
    throw TypeInfoParseError.expectedArrayLiteral(got: node)
  }
  return try arr.elements.map({ try f($0.expression) })
}

struct ArgInfo<T> {
  var name: String?
  var parser: (ExprSyntax) throws -> T

  static func stringArg(_ name: String?) -> ArgInfo<String> {
    .init(name: name, parser: parseString)
  }

  static func boolArg(_ name: String?) -> ArgInfo<Bool> {
    .init(name: name, parser: parseBool)
  }

  static func optArg<U>(
    _ name: String?, parser: @escaping (ExprSyntax) throws -> U
  ) -> ArgInfo<U?> {
    .init(
      name: name,
      parser: { node in try parseOpt(node: node, parser: parser) })
  }

  static func arrArg<U>(_ name: String?, parser: @escaping (ExprSyntax) throws -> U) -> ArgInfo<
    [U]
  > {
    .init(
      name: name,
      parser: { node in try parseArr(node: node, parser: parser) })
  }

  func toArr() -> ArgInfo<[T]> {
    .arrArg(name, parser: parser)
  }

  func toOpt() -> ArgInfo<T?> {
    .optArg(name, parser: parser)
  }

  func expect(arg: LabeledExprSyntax) throws -> T {
    let lbl = arg.label?.text
    if lbl != name {
      throw TypeInfoParseError.badArgName(expected: name, got: arg)
    }
    return try parser(arg.expression)
  }
}

extension LabeledExprListSyntax {
  func expect<each ArgType>(
    _ infos: repeat ArgInfo<each ArgType>
  ) throws -> (repeat each ArgType) {
    var lst = self.map { $0 }

    var count = 0
    for _ in repeat each infos {
      count += 1
    }

    if count != lst.count {
      throw TypeInfoParseError.argCountMismatch(expected: count, args: self)
    }

    var idx = 0

    func makeArg<T>(
      _ info: ArgInfo<T>, _ elems: inout [LabeledExprSyntax], _ idx: inout Int
    ) throws -> T {
      let val = try info.expect(arg: elems[idx])
      idx += 1
      return val
    }

    return (repeat try makeArg(each infos, &lst, &idx))
  }
}

extension NominalTypeInfo: TypeInfoSyntax {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw TypeInfoParseError.expectedFunctionCall(got: node)
    }

    guard fcall.calledExpression.trimmedDescription == "NominalTypeInfo" else {
      throw TypeInfoParseError.expectedFunctionCallNames(
        names: ["NominalTypeInfo"], got: node)
    }

    let parsed = try fcall.arguments.expect(
      .stringArg("name"),
      .init(name: "kind", parser: NominalTypeKind.fromSyntax),
      .boolArg("isUnsafe"))

    return Self(name: parsed.0, kind: parsed.1, isUnsafe: parsed.2)
  }

  public var syntax: ExprSyntax {
    """
    NominalTypeInfo(name: \(stringlit(name)), kind: \(kind.syntax), isUnsafe: \(boollit(isUnsafe)))
    """
  }
}

extension NominalTypeKind: TypeInfoSyntax {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw TypeInfoParseError.expectedFunctionCall(got: node)
    }
    switch fcall.calledExpression.trimmedDescription {
    case "structLike":
      return try .structLike(
        fcall.arguments.expect(
          ArgInfo(name: nil, parser: StructTypeInfo.fromSyntax)
        ))
    case "enumLike":
      return try .enumLike(
        fcall.arguments.expect(
          ArgInfo(name: nil, parser: EnumTypeInfo.fromSyntax)
        ))
    default:
      throw TypeInfoParseError.expectedFunctionCallNames(
        names: ["structLike", "enumLike"], got: fcall.calledExpression)
    }
  }

  public var syntax: ExprSyntax {
    switch self {
    case .enumLike(let e):
      """
      enumLike(\(e.syntax))
      """
    case .structLike(let s):
      """
      structLike(\(s.syntax))
      """
    }
  }
}

extension StructTypeInfo: TypeInfoSyntax {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw TypeInfoParseError.expectedFunctionCall(got: node)
    }
    guard fcall.calledExpression.trimmedDescription == "StructTypeInfo" else {
      throw TypeInfoParseError.expectedFunctionCallNames(
        names: ["StructTypeInfo"], got: fcall.calledExpression)
    }
    return Self(
      properties: try fcall.arguments.expect(
        .arrArg("properties", parser: StoredProperty.fromSyntax)))
  }

  public var syntax: ExprSyntax {
    """
    StructTypeInfo(properties: \(arrSyntax(properties)))
    """
  }
}

extension EnumTypeInfo: TypeInfoSyntax {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw TypeInfoParseError.expectedFunctionCall(got: node)
    }

    guard fcall.calledExpression.trimmedDescription == "EnumTypeInfo" else {
      throw TypeInfoParseError.expectedFunctionCallNames(
        names: ["EnumTypeInfo"], got: fcall.calledExpression)
    }

    let parsed = try fcall.arguments.expect(
      .boolArg("isObjC"),
      .arrArg("cases", parser: CaseInfo.fromSyntax)
    )

    return Self(isObjC: parsed.0, cases: parsed.1)
  }

  public var syntax: ExprSyntax {
    """
    EnumTypeInfo(isObjC: \(boollit(isObjC)), cases: \(arrSyntax(cases)))
    """
  }
}

extension StoredProperty: TypeInfoSyntax {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw TypeInfoParseError.expectedFunctionCall(got: node)
    }

    guard fcall.calledExpression.trimmedDescription == "StoredProperty" else {
      throw TypeInfoParseError.expectedFunctionCallNames(
        names: ["StoredProperty"], got: fcall.calledExpression)
    }

    let parsed = try fcall.arguments.expect(
      .stringArg("name"),
      .stringArg("typeName"),
      .boolArg("isVar"),
      .boolArg("isStatic"),
    )

    return Self(name: parsed.0, typeName: parsed.1, isVar: parsed.2, isStatic: parsed.3)
  }

  public var syntax: ExprSyntax {
    """
    StoredProperty(name: \(stringlit(name)), typeName: \(stringlit(typeName)), isVar: \(boollit(isVar)), isStatic: \(boollit(isStatic)))
    """
  }
}

extension CaseInfo: TypeInfoSyntax {
  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw TypeInfoParseError.expectedFunctionCall(got: node)
    }

    guard fcall.calledExpression.trimmedDescription == "CaseInfo" else {
      throw TypeInfoParseError.expectedFunctionCallNames(
        names: ["CaseInfo"], got: fcall.calledExpression)
    }

    let parsed = try fcall.arguments.expect(
      .stringArg("name"),
      .stringArg("associatedValues").toOpt().toArr()
    )

    return Self(name: parsed.0, associatedValues: parsed.1)
  }

  public var syntax: ExprSyntax {
    """
    CaseInfo(name: \(stringlit(name)), associatedValues: \(arrSyntax(associatedValues, {optSyntax($0, stringlit)})))
    """
  }
}

public protocol TypeInfoSyntax: Equatable {
  static func fromSyntax(node: ExprSyntax) throws -> Self
  var syntax: ExprSyntax { get }
}

extension TypeInfoSyntax {
  public static func fromStringLit(strlit: StringLiteralExprSyntax) throws -> Self {
    guard let underlying = strlit.representedLiteralValue else {
      throw TypeInfoParseError.expectedStrLitAsInput(got: ExprSyntax(strlit))
    }
    return try fromSyntax(node: "\(raw: underlying)")
  }

  public static func fromStringLit(expr: ExprSyntax) throws -> Self {
    guard let strlit = expr.as(StringLiteralExprSyntax.self) else {
      throw TypeInfoParseError.expectedStrLitAsInput(got: expr)
    }
    return try fromStringLit(strlit: strlit)
  }
}

func stringlit(_ str: String) -> ExprSyntax {
  ExprSyntax(StringLiteralExprSyntax(content: str))
}

func boollit(_ b: Bool) -> ExprSyntax {
  ExprSyntax(BooleanLiteralExprSyntax(booleanLiteral: b))
}

func arrSyntax<T: TypeInfoSyntax>(_ values: [T]) -> ExprSyntax {
  arrSyntax(values, \.syntax)
}

func arrSyntax<T>(_ values: [T], _ toSyntax: (T) -> ExprSyntax) -> ExprSyntax {
  ExprSyntax(ArrayExprSyntax(expressions: values.map(toSyntax)))
}

func optSyntax<T: TypeInfoSyntax>(_ value: T?) -> ExprSyntax {
  optSyntax(value, \.syntax)
}

func optSyntax<T>(_ value: T?, _ toSyntax: (T) -> ExprSyntax) -> ExprSyntax {
  if let value = value {
    toSyntax(value)
  } else {
    "nil"
  }
}
