import CASTBridging
import SwiftParser
import SwiftSyntax

extension Array {
  public func withBridgedArrayRef<T>(_ c: (BridgedArrayRef) -> T) -> T {
    withUnsafeBytes { buf in
      c(BridgedArrayRef(data: buf.baseAddress!, numElements: count))
    }
  }
}

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

  func bridged() -> ASTNodeBridged {
    switch self {
    case .expr(let e):
      return ASTNodeBridged(ptr: e, kind: .expr)
    case .stmt(let s):
      return ASTNodeBridged(ptr: s, kind: .stmt)
    case .decl(let d):
      return ASTNodeBridged(ptr: d, kind: .decl)
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

  let ctx: UnsafeMutableRawPointer
  let base: UnsafePointer<UInt8>

  @Boxed var declContext: UnsafeMutableRawPointer

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
      let loc = self.base.advanced(by: element.position.utf8Offset).raw
      let swiftASTNodes = visit(element)
      switch swiftASTNodes {
      case .decl(let d):
        out.append(d)
      case .stmt(let s):
        out.append(SwiftTopLevelCodeDecl_createStmt(ctx, declContext, loc, s, loc))
      case .expr(let e):
        out.append(SwiftTopLevelCodeDecl_createExpr(ctx, declContext, loc, e, loc))
      default:
        fatalError("Top level nodes must be decls, stmts, or exprs.")
      }
    }

    return out
  }
}

/// Generate AST nodes for all top-level entities in the given source file.
@_cdecl("swift_ASTGen_buildTopLevelASTNodes")
public func buildTopLevelASTNodes(
  sourceFilePtr: UnsafePointer<UInt8>,
  dc: UnsafeMutableRawPointer,
  ctx: UnsafeMutableRawPointer,
  outputContext: UnsafeMutableRawPointer,
  callback: @convention(c) (UnsafeMutableRawPointer, UnsafeMutableRawPointer) -> Void
) {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sourceFile in
    ASTGenVisitor(ctx: ctx, base: sourceFile.pointee.buffer.baseAddress!, declContext: dc)
      .visit(sourceFile.pointee.syntax)
      .forEach { callback($0, outputContext) }
  }
}
