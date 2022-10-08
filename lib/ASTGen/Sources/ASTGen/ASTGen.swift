import SwiftParser
import SwiftSyntax

import CASTBridging

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

/// Little utility wrapper that lets us
@propertyWrapper
class Boxed<Value> {
  var wrappedValue: Value

  init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

struct ASTGenVisitor: SyntaxTransformVisitor {
  let ctx: UnsafeMutableRawPointer
  let base: UnsafePointer<CChar>

  @Boxed var declContext: UnsafeMutableRawPointer
  
  // TODO: this some how messes up the witness table when I uncomment it locally :/
//  public func visit<T>(_ node: T?) -> [UnsafeMutableRawPointer]? {
//    if let node = node { return visit(node) }
//    return nil
//  }
  
  @_disfavoredOverload
  public func visit(_ node: SourceFileSyntax) -> UnsafeMutableRawPointer {
    fatalError("Use other overload.")
  }

  public func visitAny(_ node: Syntax) -> UnsafeMutableRawPointer {
    fatalError("Not implemented.")
  }
  
  public func visit(_ node: SourceFileSyntax) -> [UnsafeMutableRawPointer] {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var out = [UnsafeMutableRawPointer]()

    for element in node.statements {
      let swiftASTNodes = visit(element)
      if element.item.is(StmtSyntax.self) {
        out.append(SwiftTopLevelCodeDecl_createStmt(ctx, declContext, loc, swiftASTNodes, loc))
      } else if element.item.is(ExprSyntax.self) {
        out.append(SwiftTopLevelCodeDecl_createExpr(ctx, declContext, loc, swiftASTNodes, loc))
      } else {
        assert(element.item.is(DeclSyntax.self))
        out.append(swiftASTNodes)
      }
    }
    
    return out
  }
}

@_cdecl("parseTopLevelSwift")
public func parseTopLevelSwift(
    buffer: UnsafePointer<CChar>, dc: UnsafeMutableRawPointer,
    ctx: UnsafeMutableRawPointer,
    outputContext: UnsafeMutableRawPointer,
    callback: @convention(c) (UnsafeMutableRawPointer, UnsafeMutableRawPointer) -> Void
) {
  let syntax = try! Parser.parse(source: String(cString: buffer))
  ASTGenVisitor(ctx: ctx, base: buffer, declContext: dc)
    .visit(syntax)
    .forEach { callback($0, outputContext) }
}
