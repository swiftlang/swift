import ASTBridging
import BasicBridging
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

  init(
    diagnosticEngine: BridgedDiagnosticEngine,
    sourceBuffer: UnsafeBufferPointer<UInt8>,
    declContext: BridgedDeclContext,
    astContext: BridgedASTContext
  ) {
    self.diagnosticEngine = diagnosticEngine
    self.base = sourceBuffer
    self.declContext = declContext
    self.ctx = astContext
  }

  public func generate(_ node: SourceFileSyntax) -> [UnsafeMutableRawPointer] {
    var out = [UnsafeMutableRawPointer]()

    for element in node.statements {
      let loc = element.bridgedSourceLoc(in: self)
      let swiftASTNodes = generate(element)
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
  func generate(_ node: DeclSyntax) -> BridgedDecl {
    return generate(Syntax(node)).castToDecl
  }

  func generate(_ node: ExprSyntax) -> BridgedExpr {
    return generate(Syntax(node)).castToExpr
  }

  func generate(_ node: PatternSyntax) -> ASTNode {
    return generate(Syntax(node))
  }

  func generate(_ node: StmtSyntax) -> BridgedStmt {
    return generate(Syntax(node)).castToStmt
  }

  func generate(_ node: TypeSyntax) -> BridgedTypeRepr {
    return generate(Syntax(node)).castToType
  }

  func generate(_ node: some SyntaxChildChoices) -> ASTNode {
    return self.generate(Syntax(node))
  }

  func generate(_ node: Syntax) -> ASTNode {
    switch node.as(SyntaxEnum.self) {
    case .actorDecl(let node):
      return .decl(generate(node).asDecl)
    case .arrayElement(let node):
      return .expr(generate(node))
    case .arrayExpr(let node):
      return .expr(generate(node).asExpr)
    case .arrayType(let node):
      return .type(generate(node))
    case .associatedTypeDecl(let node):
      return .decl(generate(node).asDecl)
    case .attributedType(let node):
      return .type(generate(node))
    case .booleanLiteralExpr(let node):
      return .expr(generate(node).asExpr)
    case .classDecl(let node):
      return .decl(generate(node).asDecl)
    case .closureExpr(let node):
      return .expr(generate(node).asExpr)
    case .codeBlock(let node):
      return .stmt(generate(node).asStmt)
    case .codeBlockItem(let node):
      return generate(node)
    case .compositionType(let node):
      return .type(generate(node))
    case .conditionElement(let node):
      return generate(node)
    case .declReferenceExpr(let node):
      return .expr(generate(node).asExpr)
    case .deinitializerDecl(let node):
      return .decl(generate(node).asDecl)
    case .dictionaryType(let node):
      return .type(generate(node))
    case .enumCaseDecl(let node):
      return .decl(generate(node).asDecl)
    case .enumDecl(let node):
      return .decl(generate(node).asDecl)
    case .expressionStmt(let node):
      return .stmt(generate(node))
    case .extensionDecl(let node):
      return .decl(generate(node).asDecl)
    case .functionCallExpr(let node):
      return .expr(generate(node).asExpr)
    case .functionDecl(let node):
      return .decl(generate(node).asDecl)
    case .functionType(let node):
      return .type(generate(node))
    case .identifierPattern(let node):
      return .expr(generate(node).asExpr)
    case .identifierType(let node):
      return .type(generate(node))
    case .ifExpr(let node):
      return .expr(generate(node).asExpr)
    case .implicitlyUnwrappedOptionalType(let node):
      return .type(generate(node))
    case .importDecl(let node):
      return .decl(generate(node).asDecl)
    case .initializerClause(let node):
      return .expr(generate(node))
    case .initializerDecl(let node):
      return .decl(generate(node).asDecl)
    case .integerLiteralExpr(let node):
      return .expr(generate(node).asExpr)
    case .labeledExprList:
      fatalError("case does not correspond to an ASTNode")
    case .memberAccessExpr(let node):
      return .expr(generate(node).asExpr)
    case .memberBlockItem(let node):
      return .decl(generate(node))
    case .memberType(let node):
      return .type(generate(node))
    case .metatypeType(let node):
      return .type(generate(node))
    case .namedOpaqueReturnType(let node):
      return .type(generate(node))
    case .nilLiteralExpr(let node):
      return .expr(generate(node).asExpr)
    case .operatorDecl(let node):
      return .decl(generate(node).asDecl)
    case .optionalType(let node):
      return .type(generate(node))
    case .packExpansionType(let node):
      return .type(generate(node))
    case .precedenceGroupDecl(let node):
      return .decl(generate(node).asDecl)
    case .protocolDecl(let node):
      return .decl(generate(node).asDecl)
    case .returnStmt(let node):
      return .stmt(generate(node).asStmt)
    case .someOrAnyType(let node):
      return .type(generate(node))
    case .stringLiteralExpr(let node):
      return .expr(generate(node).asExpr)
    case .structDecl(let node):
      return .decl(generate(node).asDecl)
    case .tupleExpr(let node):
      return .expr(generate(node).asExpr)
    case .tupleType(let node):
      return .type(generate(node))
    case .typeAliasDecl(let node):
      return .decl(generate(node).asDecl)
    case .variableDecl(let node):
      return .decl(generate(node).asDecl)
    default:
      fatalError("not implemented")
    }
  }
}

// Misc visits.
// TODO: Some of these are called within a single file/method; we may want to move them to the respective files.
extension ASTGenVisitor {
  public func generate(_ node: MemberBlockItemSyntax) -> BridgedDecl {
    generate(node.decl)
  }

  public func generate(_ node: InitializerClauseSyntax) -> BridgedExpr {
    generate(node.value)
  }

  public func generate(_ node: ConditionElementSyntax) -> ASTNode {
    generate(node.condition)
  }

  public func generate(_ node: CodeBlockItemSyntax) -> ASTNode {
    generate(node.item)
  }

  public func generate(_ node: ArrayElementSyntax) -> BridgedExpr {
    generate(node.expression)
  }

  @inline(__always)
  func generate(_ node: CodeBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate($0).bridged }.bridgedArray(in: self)
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

    return self.generate(node)
  }

  @inline(__always)
  func generate(_ node: ExprSyntax?) -> BridgedExpr? {
    guard let node else {
      return nil
    }

    return self.generate(node)
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
  diagEnginePtr: UnsafeMutableRawPointer,
  sourceFilePtr: UnsafePointer<UInt8>,
  dc: UnsafeMutableRawPointer,
  ctx: UnsafeMutableRawPointer,
  outputContext: UnsafeMutableRawPointer,
  callback: @convention(c) (UnsafeMutableRawPointer, UnsafeMutableRawPointer) -> Void
) {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sourceFile in
    ASTGenVisitor(
      diagnosticEngine: .init(raw: diagEnginePtr),
      sourceBuffer: sourceFile.pointee.buffer,
      declContext: BridgedDeclContext(raw: dc),
      astContext: BridgedASTContext(raw: ctx)
    )
    .generate(sourceFile.pointee.syntax)
    .forEach { callback($0, outputContext) }
  }
}
