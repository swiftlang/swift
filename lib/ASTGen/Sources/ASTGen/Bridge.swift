//===--- Bridge.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftIfConfig
@_spi(RawSyntax) import SwiftSyntax

public typealias Identifier = swift.Identifier
public typealias DeclBaseName = swift.DeclBaseName

public protocol BridgedNullable: ExpressibleByNilLiteral {
  associatedtype RawPtr
  init(raw: RawPtr?)
}
extension BridgedNullable {
  public init(nilLiteral: ()) {
    self.init(raw: nil)
  }
}

extension BridgedSourceLoc: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension Identifier: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableDecl: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableExpr: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableStmt: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableTypeRepr: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullablePattern: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableGenericParamList: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableTrailingWhereClause: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableParameterList: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullablePatternBindingInitializer: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableDefaultArgumentInitializer: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableCustomAttributeInitializer: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableArgumentList: /*@retroactive*/ swiftASTGen.BridgedNullable {}
extension BridgedNullableVarDecl: /*@retroactive*/ swiftASTGen.BridgedNullable {}

extension Identifier: /*@retroactive*/ Swift.Equatable {
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
extension BridgedDecl: BridgedHasNullable {
  typealias Nullable = BridgedNullableDecl
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
extension BridgedDefaultArgumentInitializer: BridgedHasNullable {
  typealias Nullable = BridgedNullableDefaultArgumentInitializer
}
extension BridgedCustomAttributeInitializer: BridgedHasNullable {
  typealias Nullable = BridgedNullableCustomAttributeInitializer
}
extension BridgedArgumentList: BridgedHasNullable {
  typealias Nullable = BridgedNullableArgumentList
}
extension BridgedVarDecl: BridgedHasNullable {
  typealias Nullable = BridgedNullableVarDecl
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

extension BridgedLabeledStmtInfo: /*@retroactive*/ Swift.ExpressibleByNilLiteral {
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

  public mutating func withBridgedString<R>(_ body: (BridgedStringRef) throws -> R) rethrows -> R {
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
public func allocateBridgedString(
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

extension BridgedStringRef: /*@retroactive*/ Swift.ExpressibleByStringLiteral {
  public init(stringLiteral str: StaticString) {
    self.init(data: str.utf8Start, count: str.utf8CodeUnitCount)
  }
}

extension VersionTuple {
  var bridged: BridgedVersionTuple {
    switch self.components.count {
    case 4:
      return BridgedVersionTuple(CUnsignedInt(components[0]), CUnsignedInt(components[1]), CUnsignedInt(components[2]), CUnsignedInt(components[3]))
    case 3:
      return BridgedVersionTuple(CUnsignedInt(components[0]), CUnsignedInt(components[1]), CUnsignedInt(components[2]))
    case 2:
      return BridgedVersionTuple(CUnsignedInt(components[0]), CUnsignedInt(components[1]))
    case 1:
      return BridgedVersionTuple(CUnsignedInt(components[0]))
    case 0:
      return BridgedVersionTuple()
    default:
      fatalError("unsuported version form")
    }
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
  /// Obtains an `ASTContext`-owned copy of this token's text.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext`.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifier(token:)")
  @inline(__always)
  func bridgedIdentifier(in astgen: ASTGenVisitor) -> Identifier {
    astgen.generateIdentifier(self)
  }

  /// Obtains a `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen`.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifierAndSourceLoc(token:)")
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> (Identifier, BridgedSourceLoc) {
    astgen.generateIdentifierAndSourceLoc(self)
  }

  /// Obtains a `ASTContext`-owned copy of this token's text, and its bridged start location in the
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
  /// Obtains a `ASTContext`-owned copy of this token's text.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext`.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifier(token:)")
  @inline(__always)
  func bridgedIdentifier(in astgen: ASTGenVisitor) -> Identifier {
    astgen.generateIdentifier(self)
  }

  /// Obtains a `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen` excluding leading trivia.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @available(*, deprecated, message: "use ASTContext.bridgedIdentifierAndSourceLoc(token:)")
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> (Identifier, BridgedSourceLoc) {
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

extension Fingerprint {
  var bridged: BridgedFingerprint {
    BridgedFingerprint(v1: self.core.0, v2: self.core.1)
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

extension BridgedArrayRef {
  public func withElements<T, R>(ofType ty: T.Type, _ c: (UnsafeBufferPointer<T>) -> R) -> R {
    let start = data?.assumingMemoryBound(to: ty)
    let buffer = UnsafeBufferPointer(start: start, count: count)
    return c(buffer)
  }
}

/// Utility to pass Swift closure to C/C++ bridging API.
///
/// C/C++ API can call the closure via `BridgedSwiftClosure::operator()` which
/// calls `bridgedSwiftClosureCall_1(_:_:)` function below.
func withBridgedSwiftClosure(closure: (UnsafeRawPointer?) -> Void, call: (BridgedSwiftClosure) -> Void) {
  withoutActuallyEscaping(closure) { escapingClosure in
    withUnsafePointer(to: escapingClosure) { ptr in
      call(BridgedSwiftClosure(closure: ptr))
    }
  }
}

@_cdecl("swift_ASTGen_bridgedSwiftClosureCall_1")
func bridgedSwiftClosureCall_1(_ bridged: BridgedSwiftClosure, _ arg: UnsafeRawPointer?) {
  bridged.closure.assumingMemoryBound(to: ((UnsafeRawPointer?) -> Void).self).pointee(arg)
}
