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

struct ASTGenVisitor: SyntaxTransformVisitor {
  let ctx: UnsafeMutableRawPointer
  let base: UnsafePointer<CChar>

  // TOOD: we need to be up updating this.
  var declContext: UnsafeMutableRawPointer
  
  // TODO: this some how messes up the witness table when I uncomment it locally :/
//  public func visit<T>(_ node: T?) -> [UnsafeMutableRawPointer]? {
//    if let node = node { return visit(node) }
//    return nil
//  }
  
  @_disfavoredOverload
  public func visit(_ node: SourceFileSyntax) -> UnsafeMutableRawPointer {
    fatalError("Use other overload.")
  }

  public func visitAny(_ node: Syntax) -> ResultType {
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

  public func visit(_ node: FunctionCallExprSyntax) -> UnsafeMutableRawPointer {
    let args = visit(node.argumentList)
    // TODO: hack
    let callee = visit(node.calledExpression)
    return SwiftFunctionCallExpr_create(self.ctx, callee, args)
  }

  public func visit(_ node: IdentifierExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    var text = node.identifier.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return SwiftIdentifierExpr_create(ctx, id, loc)
  }

  public func visit(_ node: SimpleTypeIdentifierSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    var text = node.name.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return SimpleIdentTypeRepr_create(ctx, loc, id)
  }

  public func visit(_ node: IdentifierPatternSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    var text = node.identifier.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return SwiftIdentifierExpr_create(ctx, id, loc)
  }
  
  public func visit(_ node: MemberAccessExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    let base = visit(node.base!)
    var nameText = node.name.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }
    
    return UnresolvedDotExpr_create(ctx, base, loc, name, loc)
  }
  
  public func visit(_ node: TupleExprElementSyntax) -> UnsafeMutableRawPointer {
    visit(node.expression)
  }

  public func visit(_ node: TupleExprElementListSyntax) -> UnsafeMutableRawPointer {
    let elements = node.map(self.visit)
    
    // TODO: find correct paren locs.
    let lParenLoc = self.base.advanced(by: node.position.utf8Offset).raw
    let rParenLoc = self.base.advanced(by: node.position.utf8Offset).raw
    
    return elements.withBridgedArrayRef { elementsRef in
      SwiftTupleExpr_create(self.ctx, lParenLoc, elementsRef, rParenLoc)
    }
  }

  public func visit(_ node: VariableDeclSyntax) -> UnsafeMutableRawPointer {
    let pattern = visit(node.bindings.first!.pattern)
    let initializer = visit(node.bindings.first!.initializer!)
    
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    let isStateic = false // TODO: compute this
    let isLet = node.letOrVarKeyword.tokenKind == .letKeyword

    // TODO: don't drop "initializer" on the floor.
    return SwiftVarDecl_create(ctx, pattern, loc, isStateic, isLet, declContext)
  }
  
  public func visit(_ node: ConditionElementSyntax) -> UnsafeMutableRawPointer {
    visit(node.condition)
  }

  public func visit(_ node: CodeBlockItemSyntax) -> UnsafeMutableRawPointer {
    visit(node.item)
  }

  public func visit(_ node: CodeBlockSyntax) -> UnsafeMutableRawPointer {
    let statements = node.statements.map(self.visit)
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    return statements.withBridgedArrayRef { ref in
      BraceStmt_create(ctx, loc, ref, loc)
    }
  }

  public func visit(_ node: FunctionParameterSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    let firstName: UnsafeMutableRawPointer?
    let secondName: UnsafeMutableRawPointer?
    
    if let nodeFirstName = node.firstName {
      var text = nodeFirstName.text
      firstName = text.withUTF8 { buf in
        SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count).raw
      }
    } else {
      firstName = nil
    }
    
    if let nodeSecondName = node.secondName {
      var text = nodeSecondName.text
      secondName = text.withUTF8 { buf in
        SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count).raw
      }
    } else {
      secondName = nil
    }
    
    return ParamDecl_create(ctx, loc, loc, firstName, loc, secondName, declContext)
  }

  public func visit(_ node: FunctionDeclSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    var nameText = node.identifier.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }
    
    let body: UnsafeMutableRawPointer?
    if let nodeBody = node.body {
      body = visit(nodeBody)
    } else {
      body = nil
    }

    let returnType: UnsafeMutableRawPointer?
    if let output = node.signature.output {
      returnType = visit(output.returnType)
    } else {
      returnType = nil
    }
    
    let params = node.signature.input.parameterList.map { visit($0) }
    return params.withBridgedArrayRef { ref in
      FuncDecl_create(ctx, loc, false, loc, name, loc, false, nil, false, nil, loc, ref, loc, body, returnType, declContext)
    }
  }

  public func visit(_ node: IfStmtSyntax) -> UnsafeMutableRawPointer {
    let conditions = node.conditions.map(self.visit)
    assert(conditions.count == 1) // TODO: handle multiple conditions.
    
    let body = visit(node.body)
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    if let elseBody = node.elseBody, node.elseKeyword != nil {
      return IfStmt_create(ctx, loc, conditions.first!, body, loc, visit(elseBody))
    }
    
    return IfStmt_create(ctx, loc, conditions.first!, body, nil, nil)
  }

  public func visit(_ node: StringLiteralExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return segment.withUTF8 { buf in
      let id = SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      return SwiftStringLiteralExpr_create(ctx, id, buf.count, loc)
    }
  }

  public func visit(_ node: IntegerLiteralExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.digits.text
    return segment.withUTF8 { buf in
      let id = SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      return SwiftIntegerLiteralExpr_create(ctx, id, buf.count, loc)
    }
  }
}

@_cdecl("parseTopLevelSwift")
public func parseTopLevelSwift(
    buffer: UnsafePointer<CChar>, declContext: UnsafeMutableRawPointer,
    ctx: UnsafeMutableRawPointer,
    outputContext: UnsafeMutableRawPointer,
    callback: @convention(c) (UnsafeMutableRawPointer, UnsafeMutableRawPointer) -> Void
) {
  let syntax = try! Parser.parse(source: String(cString: buffer))
  dump(syntax)
  ASTGenVisitor(ctx: ctx, base: buffer, declContext: declContext)
    .visit(syntax)
    .forEach { callback($0, outputContext) }
}
