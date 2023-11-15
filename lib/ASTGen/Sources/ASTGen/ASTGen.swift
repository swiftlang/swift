//===--- ASTGen.swift -----------------------------------------------------===//
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
import ParseBridging
// Needed to use BumpPtrAllocator
@_spi(BumpPtrAllocator) import SwiftSyntax

import struct SwiftDiagnostics.Diagnostic

extension UnsafePointer {
  public var raw: UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: self)
  }
}

enum ASTNode {
  case decl(BridgedDecl)
  case stmt(BridgedStmt)
  case expr(BridgedExpr)
  case type(BridgedTypeRepr)

  var castToExpr: BridgedExpr {
    guard case .expr(let bridged) = self else {
      fatalError("Expected an expr")
    }
    return bridged
  }

  var castToStmt: BridgedStmt {
    guard case .stmt(let bridged) = self else {
      fatalError("Expected a stmt")
    }
    return bridged
  }

  var castToDecl: BridgedDecl {
    guard case .decl(let bridged) = self else {
      fatalError("Expected a decl")
    }
    return bridged
  }

  var castToType: BridgedTypeRepr {
    guard case .type(let bridged) = self else {
      fatalError("Expected a type")
    }
    return bridged
  }

  var bridged: BridgedASTNode {
    switch self {
    case .expr(let e):
      return BridgedASTNode(raw: e.raw, kind: .expr)
    case .stmt(let s):
      return BridgedASTNode(raw: s.raw, kind: .stmt)
    case .decl(let d):
      return BridgedASTNode(raw: d.raw, kind: .decl)
    default:
      fatalError("Must be expr, stmt, or decl.")
    }
  }

  var raw: UnsafeMutableRawPointer {
    switch self {
    case .expr(let e):
      return e.raw
    case .stmt(let s):
      return s.raw
    case .decl(let d):
      return d.raw
    case .type(let t):
      return t.raw
    }
  }
}

/// Little utility wrapper that lets us have some mutable state within
/// immutable structs, and is therefore pretty evil.
@propertyWrapper
class Boxed<Value> {
  var wrappedValue: Value

  init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

struct ASTGenVisitor {
  typealias ResultType = ASTNode

  fileprivate let diagnosticEngine: BridgedDiagnosticEngine

  let base: UnsafeBufferPointer<UInt8>

  @Boxed private(set) var declContext: BridgedDeclContext

  let ctx: BridgedASTContext

  fileprivate let allocator: SwiftSyntax.BumpPtrAllocator = .init(slabSize: 256)

  /// Fallback legacy parser used when ASTGen doesn't have the generate(_:)
  /// implementation for the AST node kind.
  let legacyParse: BridgedLegacyParser

  init(
    diagnosticEngine: BridgedDiagnosticEngine,
    sourceBuffer: UnsafeBufferPointer<UInt8>,
    declContext: BridgedDeclContext,
    astContext: BridgedASTContext,
    legacyParser: BridgedLegacyParser
  ) {
    self.diagnosticEngine = diagnosticEngine
    self.base = sourceBuffer
    self.declContext = declContext
    self.ctx = astContext
    self.legacyParse = legacyParser
  }

  public func generate(_ node: SourceFileSyntax) -> [UnsafeMutableRawPointer] {
    var out = [UnsafeMutableRawPointer]()

    for element in node.statements {
      let loc = element.bridgedSourceLoc(in: self)
      let swiftASTNodes = generate(codeBlockItem: element)
      switch swiftASTNodes {
      case .decl(let d):
        out.append(d.raw)
      case .stmt(let s):
        let topLevelDecl = BridgedTopLevelCodeDecl.createParsed(
          self.ctx,
          declContext: self.declContext,
          startLoc: loc,
          stmt: s,
          endLoc: loc
        )
        out.append(topLevelDecl.raw)
      case .expr(let e):
        let topLevelDecl = BridgedTopLevelCodeDecl.createParsed(
          self.ctx,
          declContext: self.declContext,
          startLoc: loc,
          expr: e,
          endLoc: loc
        )
        out.append(topLevelDecl.raw)
      default:
        fatalError("Top level nodes must be decls, stmts, or exprs.")
      }
    }

    return out
  }
}

extension ASTGenVisitor {
  /// Replaces the current declaration context with `declContext` for the duration of its execution, and calls `body`.
  @inline(__always)
  func withDeclContext(_ declContext: BridgedDeclContext, _ body: () -> Void) {
    let oldDeclContext = self.declContext
    self.declContext = declContext
    body()
    self.declContext = oldDeclContext
  }
}

extension ASTGenVisitor {
  /// Emits the given diagnostic via the C++ diagnostic engine.
  @inline(__always)
  func diagnose(_ diagnostic: Diagnostic) {
    emitDiagnostic(
      diagnosticEngine: self.diagnosticEngine,
      sourceFileBuffer: self.base,
      diagnostic: diagnostic,
      diagnosticSeverity: diagnostic.diagMessage.severity
    )
  }
}

extension ASTGenVisitor {
  /// Generate AST from a Syntax node. The node must be a decl, stmt, expr, or
  /// type.
  func generate(_ node: Syntax) -> ASTNode {
    if let decl = node.as(DeclSyntax.self) {
      return .decl(self.generate(decl: decl))
    }
    if let stmt = node.as(StmtSyntax.self) {
      return .stmt(self.generate(stmt: stmt))
    }
    if let expr = node.as(ExprSyntax.self) {
      return .expr(self.generate(expr: expr))
    }
    if let type = node.as(TypeSyntax.self) {
      return .type(self.generate(type: type))
    }

    // --- Special cases where `node` doesn't belong to one of the base kinds.

    // CodeBlockSyntax -> BraceStmt.
    if let node = node.as(CodeBlockSyntax.self) {
      return .stmt(self.generate(codeBlock: node).asStmt)
    }
    // CodeBlockItemSyntax -> ASTNode.
    if let node = node.as(CodeBlockItemSyntax.self) {
      return self.generate(codeBlockItem: node)
    }

    fatalError("node does not correspond to an ASTNode \(node.kind)")
  }
}

// Misc visits.
// TODO: Some of these are called within a single file/method; we may want to move them to the respective files.
extension ASTGenVisitor {
  func generate(_ node: some SyntaxChildChoices) -> ASTNode {
    return self.generate(Syntax(node))
  }

  public func generate(_ node: MemberBlockItemSyntax) -> BridgedDecl {
    generate(decl: node.decl)
  }

  public func generate(_ node: InitializerClauseSyntax) -> BridgedExpr {
    generate(expr: node.value)
  }

  public func generate(_ node: ConditionElementSyntax) -> ASTNode {
    generate(node.condition)
  }

  public func generate(codeBlockItem node: CodeBlockItemSyntax) -> ASTNode {
    generate(node.item)
  }

  public func generate(_ node: ArrayElementSyntax) -> BridgedExpr {
    generate(expr: node.expression)
  }

  @inline(__always)
  func generate(_ node: CodeBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate(codeBlockItem: $0).bridged }.bridgedArray(in: self)
  }
}

// Forwarding overloads that take optional syntax nodes. These are defined on demand to achieve a consistent
// 'self.visit(<expr>)' recursion pattern between optional and non-optional inputs.
extension ASTGenVisitor {
  @inline(__always)
  func generate(_ node: TypeSyntax?) -> BridgedTypeRepr? {
    guard let node else {
      return nil
    }

    return self.generate(type: node)
  }

  @inline(__always)
  func generate(_ node: ExprSyntax?) -> BridgedExpr? {
    guard let node else {
      return nil
    }

    return self.generate(expr: node)
  }

  @inline(__always)
  func generate(_ node: (some SyntaxChildChoices)?) -> ASTNode? {
    guard let node else {
      return nil
    }

    // This call recurses without disambiguation.
    return self.generate(node) as ASTNode
  }

  @inline(__always)
  func generate(_ node: GenericParameterClauseSyntax?) -> BridgedGenericParamList? {
    guard let node else {
      return nil
    }

    return self.generate(node)
  }

  @inline(__always)
  func generate(_ node: GenericWhereClauseSyntax?) -> BridgedTrailingWhereClause? {
    guard let node else {
      return nil
    }

    return self.generate(node)
  }

  @inline(__always)
  func generate(_ node: EnumCaseParameterClauseSyntax?) -> BridgedParameterList? {
    guard let node else {
      return nil
    }

    return self.generate(node)
  }

  @inline(__always)
  func generate(_ node: InheritedTypeListSyntax?) -> BridgedArrayRef {
    guard let node else {
      return .init()
    }

    return self.generate(node)
  }

  @inline(__always)
  func generate(_ node: PrecedenceGroupNameListSyntax?) -> BridgedArrayRef {
    guard let node else {
      return .init()
    }

    return self.generate(node)
  }
}

extension Collection {
  /// Like ``Sequence.compactMap(_:)``, but returns a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  ///
  /// - Note: The purpose of this method is to make up for the performance toll of calling ``Collection.bridgedArray``
  ///   on a ``LazyFilterSequence`` due to the `count` access.
  func compactMap<T>(in astgen: ASTGenVisitor, _ transform: (Element) -> T?) -> BridgedArrayRef {
    if self.isEmpty {
      return .init()
    }

    let baseAddress = astgen.allocator.allocate(T.self, count: self.count).baseAddress!
    do {
      // A loop instead of `initialize(from: self.lazy.compactMap(transform))` because we aren't
      // doing a great job optimizing the latter.
      var currentAddress = baseAddress
      for element in self {
        guard let transformed = transform(element) else {
          continue
        }

        currentAddress.initialize(to: transformed)
        currentAddress += 1
      }
    }

    return .init(data: baseAddress, count: self.count)
  }
}

extension LazyCollectionProtocol {
  /// Returns a copy of the collection's elements as a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  func bridgedArray(in astgen: ASTGenVisitor) -> BridgedArrayRef {
    if self.isEmpty {
      return .init()
    }

    let buffer = astgen.allocator.allocate(Element.self, count: self.count)
    _ = buffer.initialize(from: self)

    return .init(data: buffer.baseAddress, count: self.count)
  }
}

// 'ReversedCollection' does not conform to 'LazyCollectionProtocol', and cannot here because it only
// conditionally conforms to 'LazySequenceProtocol' in the standard library.
// FIXME: We could make it conform unconditionally
extension ReversedCollection {
  /// Returns a copy of the collection's elements as a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  @inline(__always)
  func bridgedArray(in astgen: ASTGenVisitor) -> BridgedArrayRef {
    self.lazy.bridgedArray(in: astgen)
  }
}

extension Optional where Wrapped: LazyCollectionProtocol {
  /// Returns a copy of the collection's elements as a `BridgedArrayRef` with a lifetime tied to that of `astgen`.
  @inline(__always)
  func bridgedArray(in astgen: ASTGenVisitor) -> BridgedArrayRef {
    guard let self else {
      return .init()
    }

    return self.bridgedArray(in: astgen)
  }
}

/// Generate AST nodes for all top-level entities in the given source file.
@_cdecl("swift_ASTGen_buildTopLevelASTNodes")
public func buildTopLevelASTNodes(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  dc: BridgedDeclContext,
  ctx: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  outputContext: UnsafeMutableRawPointer,
  callback: @convention(c) (UnsafeMutableRawPointer, UnsafeMutableRawPointer) -> Void
) {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  ASTGenVisitor(
    diagnosticEngine: diagEngine,
    sourceBuffer: sourceFile.pointee.buffer,
    declContext: dc,
    astContext: ctx,
    legacyParser: legacyParser
  )
  .generate(sourceFile.pointee.syntax)
  .forEach { callback($0, outputContext) }
}

/// Generate an AST node at the given source location. Returns the generated
/// ASTNode and mutate the pointee of `endLocPtr` to the end of the node.
private func _build<Node: SyntaxProtocol>(
  kind: Node.Type,
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)

  // Find the type syntax node.
  guard
    let node = findSyntaxNodeInSourceFile(
      sourceFilePtr: sourceFilePtr,
      // FIXME: findSyntaxNodeInSourceFile should receive `BridgedSourceLoc`.
      sourceLocationPtr: sourceLoc.getOpaquePointerValue()?.assumingMemoryBound(to: UInt8.self),
      type: Node.self,
      wantOutermost: true
    )
  else {
    // FIXME: Produce an error
    return nil
  }

  // Fill in the end location.
  endLocPtr.pointee = sourceLoc.advanced(by: node.totalLength.utf8Length)

  // Convert the syntax node.
  return ASTGenVisitor(
    diagnosticEngine: diagEngine,
    sourceBuffer: sourceFile.pointee.buffer,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser
  ).generate(Syntax(node)).raw
}

@_cdecl("swift_ASTGen_buildTypeRepr")
@usableFromInline
func buildTypeRepr(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    kind: TypeSyntax.self,
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )
}

@_cdecl("swift_ASTGen_buildDecl")
@usableFromInline
func buildDecl(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    kind: DeclSyntax.self,
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )
}

@_cdecl("swift_ASTGen_buildExpr")
@usableFromInline
func buildExpr(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    kind: ExprSyntax.self,
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )
}

@_cdecl("swift_ASTGen_buildStmt")
@usableFromInline
func buildStmt(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  sourceLoc: BridgedSourceLoc,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser,
  endLocPtr: UnsafeMutablePointer<BridgedSourceLoc>
) -> UnsafeMutableRawPointer? {
  return _build(
    kind: StmtSyntax.self,
    diagEngine: diagEngine,
    sourceFilePtr: sourceFilePtr,
    sourceLoc: sourceLoc,
    declContext: declContext,
    astContext: astContext,
    legacyParser: legacyParser,
    endLocPtr: endLocPtr
  )
}
