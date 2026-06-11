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
// deriving conformances.                                                     //
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxBuilder

/// Represents information on a type for which a macro derives a conformance
/// to a protocol.
public struct NominalTypeInfo {
  var name: String
  var kind: NominalTypeKind
  var isUnsafe: Bool
}

/// Represents the kind of nominal type this is, for the moment only structs
/// and enums are supported.
public enum NominalTypeKind {
  case enumLike(EnumTypeInfo)
  case structLike(StructTypeInfo)
}

public struct EnumTypeInfo {
  /// Does this enum have the `@objc` attribute ?
  var isObjC: Bool

  /// Information on all its cases
  var cases: [CaseInfo]
}

/// Represents information on a single case of an enumeration.
public struct CaseInfo {
  var name: String

  /// For each associated value, we have the label's name and `nil` if there
  /// isn't one
  var associatedValueLabels: [String?]
}

public struct StructTypeInfo {
  /// Information on all the struct's properties
  var properties: [StoredProperty]
}

public struct StoredProperty {
  /// name of the stored property
  var name: String

  /// Textual representation of the property's type.
  var typeName: String

  /// Wether the property was introduced with `var`
  var isVar: Bool

  /// Wether the property is static
  var isStatic: Bool
}

/// Error type thrown by the various parsign functions in case of ill-formed
/// input
public enum TypeInfoParseError: Error {
  /// Inside a function call, we expected a specific label if `expected is a
  /// string or no label if it is `nil` but we got the `got` syntax node
  /// instead.
  case badArgName(expected: String?, got: LabeledExprSyntax)

  /// Inside a function call, we expected `expected` arguments but got the ones
  /// in the `args` syntax node.
  case argCountMismatch(expected: Int, args: LabeledExprListSyntax)

  /// We were expecting to parse a string literal but found the `got` syntax
  /// node instead.
  case expectedStringLiteral(got: ExprSyntax)

  /// We were expecting to parse a boolean literal but found the `got` syntax
  /// node instead.
  case expectedBoolLiteral(got: ExprSyntax)

  /// We were expecting to parse an array literal but found the `got` syntax
  /// node instead.
  case expectedArrayLiteral(got: ExprSyntax)

  /// We were expecting to parse a function call expression but found the `got`
  /// syntax node instead.
  case expectedFunctionCall(got: ExprSyntax)

  /// We expected a function call which caller's name was in `names` but found
  /// the `got` syntax node instead.
  case expectedFunctionCallNames(names: [String], got: ExprSyntax)

  /// We expected a string literal carrying Swift syntax as payload but found
  /// the `got` syntax node instead.
  case expectedStrLitAsInput(got: ExprSyntax)
}

/// Parses a string literal and returns its contents. Throws in case of error.
func parseString(node: ExprSyntax) throws -> String {
  guard let res = node.as(StringLiteralExprSyntax.self)?.representedLiteralValue else {
    throw TypeInfoParseError.expectedStringLiteral(got: node)
  }
  return res
}

/// Parses a bool literal and returns its contents. Throws in case of error.
func parseBool(node: ExprSyntax) throws -> Bool {
  guard let lit = node.as(BooleanLiteralExprSyntax.self) else {
    throw TypeInfoParseError.expectedBoolLiteral(got: node)
  }
  return lit.trimmedDescription == "true"
}

/// Parses either `nil` or an expression with the `parser` parsing function.
/// Throws if `parser` throws
func parseOpt<T>(node: ExprSyntax, parser: (ExprSyntax) throws -> T) throws -> T? {
  // Eagerly checks if it is nil. This means that T?? can never be some(nil)
  // parsed this way as we just expect either nil or a value.
  if node.is(NilLiteralExprSyntax.self) {
    return nil
  }
  return try parser(node)
}

/// Parses an array literal containing elements parsed with the `parse` function
/// and returns them all. Will throw if it is not an array literal or if one
/// of the elements throws during parsing.
func parseArr<T>(node: ExprSyntax, parser: (ExprSyntax) throws -> T) throws -> [T] {
  guard let arr = node.as(ArrayExprSyntax.self) else {
    throw TypeInfoParseError.expectedArrayLiteral(got: node)
  }
  return try arr.elements.map({ try parser($0.expression) })
}

/// Helper struct to represent an function call argument to be parsed.
struct ArgInfo<T> {
  /// The label of the argument, `nil` if there is none.
  var name: String?

  /// The parsing function
  var parser: (ExprSyntax) throws -> T

  /// Returns a string argument with the `name` label.
  static func stringArg(_ name: String?) -> ArgInfo<String> {
    .init(name: name, parser: parseString)
  }

  /// Returns a boolean argument with the `name` label.
  static func boolArg(_ name: String?) -> ArgInfo<Bool> {
    .init(name: name, parser: parseBool)
  }

  /// Returns an argument with the `name` label of type `U?` where `U`
  /// expressions can be parsed using the `parser` function.
  static func optArg<U>(
    _ name: String?, parser: @escaping (ExprSyntax) throws -> U
  ) -> ArgInfo<U?> {
    .init(
      name: name,
      parser: { node in try parseOpt(node: node, parser: parser) })
  }

  /// Returns an argument with the `name` label of type `[U]` where `U`
  /// expressions can be parsed using the `parser` function.
  static func arrArg<U>(_ name: String?, parser: @escaping (ExprSyntax) throws -> U) -> ArgInfo<
    [U]
  > {
    .init(
      name: name,
      parser: { node in try parseArr(node: node, parser: parser) })
  }

  /// Returns a new argument with the same name but expecting an array of the
  /// elements expected by `self`. This is meant to be used like a builder.
  func toArr() -> ArgInfo<[T]> {
    .arrArg(name, parser: parser)
  }

  /// Returns a new argument with the same name but expecting an optional of the
  /// elements expected by `self`. This is meant to be used like a builder.
  func toOpt() -> ArgInfo<T?> {
    .optArg(name, parser: parser)
  }

  /// Returns the parsed argument from `arg` and throws if the name is
  /// mismatched or if the parsing function fails.
  func expect(arg: LabeledExprSyntax) throws -> T {
    let lbl = arg.label?.text
    if lbl != name {
      throw TypeInfoParseError.badArgName(expected: name, got: arg)
    }
    return try parser(arg.expression)
  }
}

func arityOf<each T>(_ args: repeat each T) -> Int {
  var n = 0
  func increment<U>(_: U) { n += 1 }
  _ = (repeat increment(each args))
  return n
}

extension LabeledExprListSyntax {

  /// Given a LabeledExprListSyntax and a variable number of argument infos,
  /// try to parse the argument list as expected by the infos. Throws if one
  /// of the arguments fails to parse.
  func expect<each ArgType>(
    _ infos: repeat ArgInfo<each ArgType>
  ) throws -> (repeat each ArgType) {
    // Building the list of arguments syntax
    var lst = self.map { $0 }
    let count = arityOf(repeat each infos)

    // Throw if there is a count mismatch, no need to try parsing anything.
    if count != lst.count {
      throw TypeInfoParseError.argCountMismatch(expected: count, args: self)
    }

    var idx = 0

    /// Helper function to parse a single argument and increment the count to
    /// be able to parse the next one. This could not be a closure as it is
    /// generic in the type of the parsed expression `T`.
    func makeArg<T>(
      _ info: ArgInfo<T>, _ elems: inout [LabeledExprSyntax], _ idx: inout Int
    ) throws -> T {
      let val = try info.expect(arg: elems[idx])
      idx += 1
      return val
    }

    // For each argument, try and parse it. If all goes well a tuple containing
    // all args is returned.
    return (repeat try makeArg(each infos, &lst, &idx))
  }
}

/// Protocol for `NominalTypeInfo` and associated types to conform to.
public protocol TypeInfoSyntax: Equatable {
  /// Parses `node` into `Self` and throws if an error occured
  static func fromSyntax(node: ExprSyntax) throws -> Self

  /// Builds a syntax node that can be parsed again to the same value as `self`.
  var syntax: ExprSyntax { get }
}

extension NominalTypeInfo: TypeInfoSyntax {

  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    // Expecting:
    //   NominalTypeInfo(
    //       name: <String>,
    //       kind: <NominalTypeKind>,
    //       isUnsafe: <Bool>)

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
    // Expecting:
    //   NominalTypeKind(structLike(<StructTypeInfo>))
    // or
    //   NominalTypeKind(enumLike(<EnumTypeInfo>))

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
    // Expecting:
    //   StructTypeInfo(properties: <[StoredProperty]>)

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
    // Expecting:
    //   EnumTypeInfo(isObjC: <Bool>, cases: <[CaseInfo]>)

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
    // Expecting:
    //   StoredProperty(
    //       name: <String>,
    //       typeName: <String>,
    //       isVar: <Bool>,
    //       isStatic: <Bool>)

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
      .boolArg("isStatic"))

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
    // Expecting:
    //   CaseInfo(name: <String>, associatedValues: <[String?]>)

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

extension TypeInfoSyntax {

  /// Returns the parsed `Self` from a payloaded string literal containing
  /// Swift syntax.
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

/// Creates a string literal syntax node with `str` contents.
func stringlit(_ str: String) -> ExprSyntax {
  ExprSyntax(StringLiteralExprSyntax(content: str))
}

/// Creates a bool literal syntax node with the value `b`.
func boollit(_ b: Bool) -> ExprSyntax {
  ExprSyntax(BooleanLiteralExprSyntax(booleanLiteral: b))
}

/// Creates an array syntax node, with the element values from which we can
/// derive syntax.
func arrSyntax<T: TypeInfoSyntax>(_ values: [T]) -> ExprSyntax {
  arrSyntax(values, \.syntax)
}

/// Creates an array syntax node, with the element values from the mapping of
/// `values` by the `toSyntax` function.
func arrSyntax<T>(_ values: [T], _ toSyntax: (T) -> ExprSyntax) -> ExprSyntax {
  ExprSyntax(ArrayExprSyntax(expressions: values.map(toSyntax)))
}

/// Creates a `nil` syntax node if `value` is `nil` and the derived syntax of
/// `value` otherwise.
func optSyntax<T: TypeInfoSyntax>(_ value: T?) -> ExprSyntax {
  optSyntax(value, \.syntax)
}

/// Creates a `nil` syntax node if `value` is `nil` and the syntax node
/// produced by calling `toSyntax` on `value` otherwise.
func optSyntax<T>(_ value: T?, _ toSyntax: (T) -> ExprSyntax) -> ExprSyntax {
  if let value = value {
    toSyntax(value)
  } else {
    "nil"
  }
}
