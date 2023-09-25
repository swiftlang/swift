import CASTBridging
import CBasicBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
// Needed to use BumpPtrAllocator
@_spi(RawSyntax)
import SwiftSyntax
import struct SwiftDiagnostics.Diagnostic

extension UnsafePointer {
  public var raw: UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: self)
  }
}

enum ASTNode {
  case decl(UnsafeMutableRawPointer)
  case stmt(UnsafeMutableRawPointer)
  case expr(UnsafeMutableRawPointer)
  case type(UnsafeMutableRawPointer)
  case misc(UnsafeMutableRawPointer)

  var rawValue: UnsafeMutableRawPointer {
    switch self {
    case .decl(let ptr):
      return ptr
    case .stmt(let ptr):
      return ptr
    case .expr(let ptr):
      return ptr
    case .type(let ptr):
      return ptr
    case .misc(let ptr):
      return ptr
    }
  }

  var bridged: BridgedASTNode {
    switch self {
    case .expr(let e):
      return BridgedASTNode(ptr: e, kind: .expr)
    case .stmt(let s):
      return BridgedASTNode(ptr: s, kind: .stmt)
    case .decl(let d):
      return BridgedASTNode(ptr: d, kind: .decl)
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

struct ASTGenVisitor: SyntaxTransformVisitor {
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

  // TODO: this some how messes up the witness table when I uncomment it locally :/
  //  public func visit<T>(_ node: T?) -> [UnsafeMutableRawPointer]? {
  //    if let node = node { return visit(node) }
  //    return nil
  //  }

  @_disfavoredOverload
  public func visit(_ node: SourceFileSyntax) -> ASTNode {
    fatalError("Use other overload.")
  }

  public func visitAny(_ node: Syntax) -> ASTNode {
    fatalError("Not implemented.")
  }

  public func visit(_ node: SourceFileSyntax) -> [UnsafeMutableRawPointer] {
    var out = [UnsafeMutableRawPointer]()

    for element in node.statements {
      let loc = element.bridgedSourceLoc(in: self)
      let swiftASTNodes = visit(element)
      switch swiftASTNodes {
      case .decl(let d):
        out.append(d)
      case .stmt(let s):
        out.append(TopLevelCodeDecl_createStmt(astContext: self.ctx, declContext: self.declContext, startLoc: loc, statement: s, endLoc: loc))
      case .expr(let e):
        out.append(TopLevelCodeDecl_createExpr(astContext: self.ctx, declContext: self.declContext, startLoc: loc, expression: e, endLoc: loc))
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

// Misc visits.
// TODO: Some of these are called within a single file/method; we may want to move them to the respective files.
extension ASTGenVisitor {
  public func visit(_ node: MemberBlockItemSyntax) -> ASTNode {
    visit(Syntax(node.decl))
  }

  public func visit(_ node: InitializerClauseSyntax) -> ASTNode {
    visit(node.value)
  }

  public func visit(_ node: ConditionElementSyntax) -> ASTNode {
    visit(node.condition)
  }

  public func visit(_ node: CodeBlockItemSyntax) -> ASTNode {
    visit(node.item)
  }

  public func visit(_ node: ArrayElementSyntax) -> ASTNode {
    visit(node.expression)
  }

  @inline(__always)
  func visit(_ node: CodeBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.visit($0).bridged }.bridgedArray(in: self)
  }
}

// Forwarding overloads that take optional syntax nodes. These are defined on demand to achieve a consistent
// 'self.visit(<expr>)' recursion pattern between optional and non-optional inputs.
extension ASTGenVisitor {
  @inline(__always)
  func visit(_ node: TypeSyntax?) -> ASTNode? {
    guard let node else {
      return nil
    }

    return self.visit(node)
  }

  @inline(__always)
  func visit(_ node: ExprSyntax?) -> ASTNode? {
    guard let node else {
      return nil
    }

    return self.visit(node)
  }

  @inline(__always)
  func visit(_ node: (some SyntaxChildChoices)?) -> ASTNode? {
    guard let node else {
      return nil
    }

    // This call recurses without disambiguation.
    return (self.visit as (_) -> ASTNode)(node)
  }

  @inline(__always)
  func visit(_ node: GenericParameterClauseSyntax?) -> ASTNode? {
    guard let node else {
      return nil
    }

    return self.visit(node)
  }

  @inline(__always)
  func visit(_ node: GenericWhereClauseSyntax?) -> ASTNode? {
    guard let node else {
      return nil
    }

    return self.visit(node)
  }

  @inline(__always)
  func visit(_ node: EnumCaseParameterClauseSyntax?) -> ASTNode? {
    guard let node else {
      return nil
    }

    return self.visit(node)
  }

  @inline(__always)
  func visit(_ node: InheritedTypeListSyntax?) -> BridgedArrayRef {
    guard let node else {
      return .init()
    }

    return self.visit(node)
  }

  @inline(__always)
  func visit(_ node: PrecedenceGroupNameListSyntax?) -> BridgedArrayRef {
    guard let node else {
      return .init()
    }

    return self.visit(node)
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

    return .init(data: baseAddress, numElements: SwiftInt(self.count))
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

    return .init(data: buffer.baseAddress, numElements: SwiftInt(self.count))
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
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
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
    .visit(sourceFile.pointee.syntax)
    .forEach { callback($0, outputContext) }
  }
}
