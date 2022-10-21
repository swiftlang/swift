import SwiftSyntax
@_spi(Testing) import _SwiftSyntaxMacros

extension SyntaxProtocol {
  func token(at position: AbsolutePosition) -> TokenSyntax? {
    // If the position isn't within this node at all, return early.
    guard position >= self.position && position < self.endPosition else {
      return nil
    }

    // If we are a token syntax, that's it!
    if let token = Syntax(self).as(TokenSyntax.self) {
      return token
    }

    // Otherwise, it must be one of our children.
    return children(viewMode: .sourceAccurate).lazy.compactMap { child in
      child.token(at: position)
    }.first
  }
}

@_cdecl("swift_ASTGen_printMacroResult")
public func printMacroResult(
  sourceFilePtr: UnsafeRawPointer,
  sourceLocationPtr: UnsafePointer<UInt8>?
) -> Int {
  guard let sourceLocationPtr = sourceLocationPtr else {
    print("NULL source location")
    return -1
  }

  return sourceFilePtr.withMemoryRebound(
    to: ExportedSourceFile.self, capacity: 1
  ) { (sourceFile: UnsafePointer<ExportedSourceFile>) -> Int in
    // Find the offset.
    let buffer = sourceFile.pointee.buffer
    let offset = sourceLocationPtr - buffer.baseAddress!
    if offset < 0 || offset >= buffer.count {
      print("source location isn't inside this buffer")
      return -1
    }

    let sf = sourceFile.pointee.syntax
    guard let token = sf.token(at: AbsolutePosition(utf8Offset: offset)) else {
      print("couldn't find token at offset \(offset)")
      return -1
    }

    guard let parentSyntax = token.parent,
          parentSyntax.is(MacroExpansionExprSyntax.self) else {
      print("not on a macro expansion node: \(token.recursiveDescription)")
      return -1
    }

    let macroSystem = MacroSystem.exampleSystem
    let converter = SourceLocationConverter(
      file: sourceFile.pointee.fileName, tree: sf
    )
    let context = MacroEvaluationContext(
      moduleName: sourceFile.pointee.moduleName,
      sourceLocationConverter: converter
    )

    let evaluatedSyntax = parentSyntax.evaluateMacro(
      with: macroSystem, context: context
    ) { error in
      /* TODO: Report errors */
    }

    print("Macro rewrite: \(parentSyntax.withoutTrivia()) --> \(evaluatedSyntax.withoutTrivia())")

    return 0
  }
}
