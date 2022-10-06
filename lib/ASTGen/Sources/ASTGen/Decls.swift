import SwiftParser
import SwiftSyntax

import CASTBridging

extension ASTGenVisitor {
  public func visit(_ node: StructDeclSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var nameText = node.identifier.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }
    
    let out = StructDecl_create(ctx, loc, name, loc, declContext)
    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    node.members.members.map(self.visit).withBridgedArrayRef { ref in
      NominalTypeDecl_setMembers(out.nominalDecl, ref)
    }
    
    return out.decl
  }
  
  public func visit(_ node: ClassDeclSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var nameText = node.identifier.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    let out = ClassDecl_create(ctx, loc, name, loc, declContext)
    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    node.members.members.map(self.visit).withBridgedArrayRef { ref in
      NominalTypeDecl_setMembers(out.nominalDecl, ref)
    }
    
    return out.decl
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

  public func visit(_ node: CodeBlockSyntax) -> UnsafeMutableRawPointer {
    let statements = node.statements.map(self.visit)
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    return statements.withBridgedArrayRef { ref in
      BraceStmt_createStmt(ctx, loc, ref, loc)
    }
  }

  public func visit(_ node: FunctionParameterSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    let firstName: UnsafeMutableRawPointer?
    let secondName: UnsafeMutableRawPointer?
    
    if let nodeFirstName = node.firstName {
      var text = nodeFirstName.text
      firstName = text.withUTF8 { buf in
        SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      }
    } else {
      firstName = nil
    }
    
    if let nodeSecondName = node.secondName {
      var text = nodeSecondName.text
      secondName = text.withUTF8 { buf in
        SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
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
}
