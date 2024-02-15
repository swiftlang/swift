//===--- Bridge.swift -----------------------------------------------------===//
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

import ASTBridging
import BasicBridging
@_spi(RawSyntax) import SwiftSyntax

protocol BridgedNullable: ExpressibleByNilLiteral {
  associatedtype RawPtr
  init(raw: RawPtr?)
}
extension BridgedNullable {
  public init(nilLiteral: ()) {
    self.init(raw: nil)
  }
}

extension BridgedSourceLoc: BridgedNullable {}
extension BridgedIdentifier: BridgedNullable {}
extension BridgedNullableExpr: BridgedNullable {}
extension BridgedNullableStmt: BridgedNullable {}
extension BridgedNullableTypeRepr: BridgedNullable {}
extension BridgedNullablePattern: BridgedNullable {}
extension BridgedNullableGenericParamList: BridgedNullable {}
extension BridgedNullableTrailingWhereClause: BridgedNullable {}
extension BridgedNullableParameterList: BridgedNullable {}
extension BridgedNullablePatternBindingInitializer: BridgedNullable {}

extension BridgedIdentifier: Equatable {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    lhs.raw == rhs.raw
  }
}

/// Protocol that declares that there's a "Nullable" variation of the type.
///
/// E.g. BridgedExpr vs BridgedNullableExpr.
protocol BridgedHasNullable {
  associatedtype Nullable: BridgedNullable
  var raw: Nullable.RawPtr { get }
}
extension Optional where Wrapped: BridgedHasNullable {
  /// Convert an Optional to Nullable variation of the wrapped type.
  var asNullable: Wrapped.Nullable {
    Wrapped.Nullable(raw: self?.raw)
  }
}

extension BridgedStmt: BridgedHasNullable {
  typealias Nullable = BridgedNullableStmt
}
extension BridgedExpr: BridgedHasNullable {
  typealias Nullable = BridgedNullableExpr
}
extension BridgedTypeRepr: BridgedHasNullable {
  typealias Nullable = BridgedNullableTypeRepr
}
extension BridgedPattern: BridgedHasNullable {
  typealias Nullable = BridgedNullablePattern
}
extension BridgedGenericParamList: BridgedHasNullable {
  typealias Nullable = BridgedNullableGenericParamList
}
extension BridgedTrailingWhereClause: BridgedHasNullable {
  typealias Nullable = BridgedNullableTrailingWhereClause
}
extension BridgedParameterList: BridgedHasNullable {
  typealias Nullable = BridgedNullableParameterList
}
extension BridgedPatternBindingInitializer: BridgedHasNullable {
  typealias Nullable = BridgedNullablePatternBindingInitializer
}

public extension BridgedSourceLoc {
  /// Form a source location at the given absolute position in `buffer`.
  init(
    at position: AbsolutePosition,
    in buffer: UnsafeBufferPointer<UInt8>
  ) {
    precondition(position.utf8Offset >= 0 && position.utf8Offset <= buffer.count)
    self = BridgedSourceLoc(raw: buffer.baseAddress!).advanced(by: position.utf8Offset)
  }
}

extension BridgedLabeledStmtInfo: ExpressibleByNilLiteral {
  public init(nilLiteral: ()) {
    self.init()
  }
}

extension String {
  init(bridged: BridgedStringRef) {
    self.init(
      decoding: UnsafeBufferPointer(start: bridged.data, count: bridged.count),
      as: UTF8.self
    )
  }

  mutating func withBridgedString<R>(_ body: (BridgedStringRef) throws -> R) rethrows -> R {
    try withUTF8 { buffer in
      try body(BridgedStringRef(data: buffer.baseAddress, count: buffer.count))
    }
  }
}

extension SyntaxText {
  var bridged: BridgedStringRef {
    BridgedStringRef(data: self.baseAddress, count: self.count)
  }
}

/// Allocate a copy of the given string as a null-terminated UTF-8 string.
func allocateBridgedString(
  _ string: String
) -> BridgedStringRef {
  var string = string
  return string.withUTF8 { utf8 in
    let ptr = UnsafeMutablePointer<UInt8>.allocate(
      capacity: utf8.count + 1
    )
    if let baseAddress = utf8.baseAddress {
      ptr.initialize(from: baseAddress, count: utf8.count)
    }

    // null terminate, for client's convenience.
    ptr[utf8.count] = 0

    return BridgedStringRef(data: ptr, count: utf8.count)
  }
}

@_cdecl("swift_ASTGen_freeBridgedString")
public func freeBridgedString(bridged: BridgedStringRef) {
  bridged.data?.deallocate()
}

extension BridgedStringRef {
  var isEmptyInitialized: Bool {
    return self.data == nil && self.count == 0
  }
}

extension BridgedStringRef: ExpressibleByStringLiteral {
  public init(stringLiteral str: StaticString) {
    self.init(data: str.utf8Start, count: str.utf8CodeUnitCount)
  }
}

extension SyntaxProtocol {
  /// Obtains the bridged start location of the node excluding leading trivia in the source buffer provided by `astgen`
  ///
  /// - Parameter astgen: The visitor providing the source buffer.
  @available(*, deprecated, message: "use ASTContext.bridgedSourceLoc(syntax:)")
  @inline(__always)
  func bridgedSourceLoc(in astgen: ASTGenVisitor) -> BridgedSourceLoc {
    astgen.generateSourceLoc(self)
  }
}

extension Optional where Wrapped: SyntaxProtocol {
  /// Obtains the bridged start location of the node excluding leading trivia in the source buffer provided by `astgen`.
  ///
  /// - Parameter astgen: The visitor providing the source buffer.
  @available(*, deprecated, message: "use ASTContext.bridgedSourceLoc(syntax:)")
  @inline(__always)
  func bridgedSourceLoc(in astgen: ASTGenVisitor) -> BridgedSourceLoc {
    astgen.generateSourceLoc(self)
  }
}

extension TokenSyntax {
  /// Obtains a bridged, `ASTContext`-owned copy of this token's text.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext`.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifier(token:)")
  @inline(__always)
  func bridgedIdentifier(in astgen: ASTGenVisitor) -> BridgedIdentifier {
    astgen.generateIdentifier(self)
  }

  /// Obtains a bridged, `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen`.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifierAndSourceLoc(token:)")
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> (BridgedIdentifier, BridgedSourceLoc) {
    astgen.generateIdentifierAndSourceLoc(self)
  }

  /// Obtains a bridged, `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen`.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifierAndSourceLoc(token:)")
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> BridgedLocatedIdentifier {
    astgen.generateLocatedIdentifier(self)
  }
}

extension Optional<TokenSyntax> {
  /// Obtains a bridged, `ASTContext`-owned copy of this token's text.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext`.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifier(token:)")
  @inline(__always)
  func bridgedIdentifier(in astgen: ASTGenVisitor) -> BridgedIdentifier {
    astgen.generateIdentifier(self)
  }

  /// Obtains a bridged, `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen` excluding leading trivia.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifierAndSourceLoc(token:)")
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> (BridgedIdentifier, BridgedSourceLoc) {
    astgen.generateIdentifierAndSourceLoc(self)
  }
}

extension BridgedSourceRange {
  @available(*, deprecated, message: "use ASTContext.bridgedSourceRange(startToken:endToken:)")
  @inline(__always)
  init(startToken: TokenSyntax, endToken: TokenSyntax, in astgen: ASTGenVisitor) {
    self = astgen.generateSourceRange(start: startToken, end: endToken)
  }
}

/// Helper collection type that lazily concatenates two collections.
struct ConcatCollection<C1: Collection, C2: Collection> where C1.Element == C2.Element {
  let c1: C1
  let c2: C2

  init(_ c1: C1, _ c2: C2) {
    self.c1 = c1
    self.c2 = c2
  }
}

extension ConcatCollection: LazyCollectionProtocol {
  typealias Element = C1.Element
  enum Index: Comparable {
    case c1(C1.Index)
    case c2(C2.Index)
  }

  var count: Int { c1.count + c2.count }

  private func _normalizedIndex(_ i: C1.Index) -> Index {
    return i != c1.endIndex ? .c1(i) : .c2(c2.startIndex)
  }

  var startIndex: Index { _normalizedIndex(c1.startIndex) }
  var endIndex: Index { .c2(c2.endIndex) }

  func index(after i: Index) -> Index {
    switch i {
    case .c1(let i): return _normalizedIndex(c1.index(after: i))
    case .c2(let i): return .c2(c2.index(after: i))
    }
  }

  subscript(i: Index) -> Element {
    switch i {
    case .c1(let i): return c1[i]
    case .c2(let i): return c2[i]
    }
  }
}
