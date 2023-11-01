import ASTBridging
import BasicBridging
import SwiftSyntax

extension BridgedSourceLoc: ExpressibleByNilLiteral {
  public init(nilLiteral: ()) {
    self.init(raw: nil)
  }
}

extension BridgedIdentifier: ExpressibleByNilLiteral {
  public init(nilLiteral: ()) {
    self.init(raw: nil)
  }
}

extension BridgedStmt? {
  var asNullable: BridgedNullableStmt {
    .init(raw: self?.raw)
  }
}

extension BridgedExpr? {
  var asNullable: BridgedNullableExpr {
    .init(raw: self?.raw)
  }
}

extension BridgedTypeRepr? {
  var asNullable: BridgedNullableTypeRepr {
    .init(raw: self?.raw)
  }
}

extension BridgedGenericParamList? {
  var asNullable: BridgedNullableGenericParamList {
    .init(raw: self?.raw)
  }
}

extension BridgedTrailingWhereClause? {
  var asNullable: BridgedNullableTrailingWhereClause {
    .init(raw: self?.raw)
  }
}

extension BridgedParameterList? {
  var asNullable: BridgedNullableParameterList {
    .init(raw: self?.raw)
  }
}

extension BridgedSourceLoc {
  /// Form a source location at the given absolute position in `buffer`.
  init(
    at position: AbsolutePosition,
    in buffer: UnsafeBufferPointer<UInt8>
  ) {
    precondition(position.utf8Offset >= 0 && position.utf8Offset <= buffer.count)
    self = BridgedSourceLoc(raw: buffer.baseAddress!).advanced(by: position.utf8Offset)
  }
}

extension BridgedSourceRange {
  @inline(__always)
  init(startToken: TokenSyntax, endToken: TokenSyntax, in astgen: ASTGenVisitor) {
    self.init(start: startToken.bridgedSourceLoc(in: astgen), end: endToken.bridgedSourceLoc(in: astgen))
  }
}

extension String {
  mutating func withBridgedString<R>(_ body: (BridgedStringRef) throws -> R) rethrows -> R {
    try withUTF8 { buffer in
      try body(BridgedStringRef(data: buffer.baseAddress, count: buffer.count))
    }
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

extension SyntaxProtocol {
  /// Obtains the bridged start location of the node excluding leading trivia in the source buffer provided by `astgen`
  ///
  /// - Parameter astgen: The visitor providing the source buffer.
  @inline(__always)
  func bridgedSourceLoc(in astgen: ASTGenVisitor) -> BridgedSourceLoc {
    return BridgedSourceLoc(at: self.positionAfterSkippingLeadingTrivia, in: astgen.base)
  }
}

extension Optional where Wrapped: SyntaxProtocol {
  /// Obtains the bridged start location of the node excluding leading trivia in the source buffer provided by `astgen`.
  ///
  /// - Parameter astgen: The visitor providing the source buffer.
  @inline(__always)
  func bridgedSourceLoc(in astgen: ASTGenVisitor) -> BridgedSourceLoc {
    guard let self else {
      return nil
    }

    return self.bridgedSourceLoc(in: astgen)
  }
}

extension TokenSyntax {
  /// Obtains a bridged, `ASTContext`-owned copy of this token's text.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext`.
  @inline(__always)
  func bridgedIdentifier(in astgen: ASTGenVisitor) -> BridgedIdentifier {
    var text = self.text
    return text.withBridgedString { bridged in
      astgen.ctx.getIdentifier(bridged)
    }
  }

  /// Obtains a bridged, `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen`.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> (BridgedIdentifier, BridgedSourceLoc) {
    return (self.bridgedIdentifier(in: astgen), self.bridgedSourceLoc(in: astgen))
  }

  /// Obtains a bridged, `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen`.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> BridgedIdentifierAndSourceLoc {
    let (name, nameLoc) = self.bridgedIdentifierAndSourceLoc(in: astgen)
    return .init(name: name, nameLoc: nameLoc)
  }
}

extension Optional<TokenSyntax> {
  /// Obtains a bridged, `ASTContext`-owned copy of this token's text.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext`.
  @inline(__always)
  func bridgedIdentifier(in astgen: ASTGenVisitor) -> BridgedIdentifier {
    guard let self else {
      return nil
    }

    return self.bridgedIdentifier(in: astgen)
  }

  /// Obtains a bridged, `ASTContext`-owned copy of this token's text, and its bridged start location in the
  /// source buffer provided by `astgen` excluding leading trivia.
  ///
  /// - Parameter astgen: The visitor providing the `ASTContext` and source buffer.
  @inline(__always)
  func bridgedIdentifierAndSourceLoc(in astgen: ASTGenVisitor) -> (BridgedIdentifier, BridgedSourceLoc) {
    guard let self else {
      return (nil, nil)
    }

    return self.bridgedIdentifierAndSourceLoc(in: astgen)
  }
}
