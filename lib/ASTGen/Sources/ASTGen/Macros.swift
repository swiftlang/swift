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

@_cdecl("swift_ASTGen_evaluateMacro")
public func evaluateMacro(
  sourceFilePtr: UnsafePointer<UInt8>,
  sourceLocationPtr: UnsafePointer<UInt8>?,
  expandedSourcePointer: UnsafeMutablePointer<UnsafePointer<UInt8>?>,
  expandedSourceLength: UnsafeMutablePointer<Int>
) -> Int {
  // We didn't expand anything so far.
  expandedSourcePointer.pointee = nil
  expandedSourceLength.pointee = 0

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

    var evaluatedSyntaxStr = evaluatedSyntax.withoutTrivia().description
    evaluatedSyntaxStr.withUTF8 { utf8 in
      let evaluatedResultPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: utf8.count + 1)
      if let baseAddress = utf8.baseAddress {
        evaluatedResultPtr.initialize(from: baseAddress, count: utf8.count)
      }
      evaluatedResultPtr[utf8.count] = 0

      expandedSourcePointer.pointee = UnsafePointer(evaluatedResultPtr)
      expandedSourceLength.pointee = utf8.count + 1
    }

    return 0
  }
}
