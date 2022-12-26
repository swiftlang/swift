import SwiftDiagnostics
import SwiftParser
import SwiftSyntax
import _SwiftSyntaxMacros

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

/// Describes a macro that has been "exported" to the C++ part of the
/// compiler, with enough information to interface with the C++ layer.
struct ExportedMacro {
  var macro: Macro.Type
}

/// Resolve a reference to type metadata into a macro, if posible.
///
/// Returns an unmanaged pointer to an ExportedMacro instance that describes
/// the specified macro. If there is no macro with the given name, produces
/// nil.
@_cdecl("swift_ASTGen_resolveMacroType")
public func resolveMacroType(
  macroTypePtr: UnsafePointer<UInt8>
) -> UnsafeRawPointer? {
  let macroType = unsafeBitCast(macroTypePtr, to: Any.Type.self)

  guard let macro = macroType as? Macro.Type else {
    return nil
  }

  // Allocate and initialize the exported macro.
  let exportedPtr = UnsafeMutablePointer<ExportedMacro>.allocate(capacity: 1)
  exportedPtr.initialize(to: .init(macro: macro))
  return UnsafeRawPointer(exportedPtr)
}

/// Destroys the given macro.
@_cdecl("swift_ASTGen_destroyMacro")
public func destroyMacro(
  macroPtr: UnsafeMutablePointer<UInt8>
) {
  macroPtr.withMemoryRebound(to: ExportedMacro.self, capacity: 1) { macro in
    macro.deinitialize(count: 1)
    macro.deallocate()
  }
}

/// Allocate a copy of the given string as a UTF-8 string.
private func allocateUTF8String(
  _ string: String,
  nullTerminated: Bool = false
) -> (UnsafePointer<UInt8>, Int) {
  var string = string
  return string.withUTF8 { utf8 in
    let capacity = utf8.count + (nullTerminated ? 1 : 0)
    let ptr = UnsafeMutablePointer<UInt8>.allocate(
      capacity: capacity
    )
    if let baseAddress = utf8.baseAddress {
      ptr.initialize(from: baseAddress, count: utf8.count)
    }

    if nullTerminated {
      ptr[utf8.count] = 0
    }

    return (UnsafePointer<UInt8>(ptr), utf8.count)
  }
}

extension String {
  /// Drop everything up to and including the last '/' from the string.
  fileprivate func withoutPath() -> String {
    // Only keep everything after the last slash.
    if let lastSlash = lastIndex(of: "/") {
      return String(self[index(after: lastSlash)...])
    }

    return self
  }
}

/// Diagnostic message used for thrown errors.
fileprivate struct ThrownErrorDiagnostic: DiagnosticMessage {
  let message: String

  var severity: DiagnosticSeverity { .error }

  var diagnosticID: MessageID {
    .init(domain: "SwiftSyntaxMacros", id: "ThrownErrorDiagnostic")
  }
}

@_cdecl("swift_ASTGen_evaluateMacro")
@usableFromInline
func evaluateMacro(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  macroPtr: UnsafeMutablePointer<UInt8>,
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
          let parentExpansion = parentSyntax.as(MacroExpansionExprSyntax.self)
    else {
      print("not on a macro expansion node: \(token.recursiveDescription)")
      return -1
    }

    var context = MacroExpansionContext(
      moduleName: sourceFile.pointee.moduleName,
      fileName: sourceFile.pointee.fileName.withoutPath()
    )

    let evaluatedSyntax: ExprSyntax = macroPtr.withMemoryRebound(to: ExportedMacro.self, capacity: 1) { macro in
      guard let exprMacro = macro.pointee.macro as? ExpressionMacro.Type else {
        print("not an expression macro")
        return ExprSyntax(parentExpansion)
      }

      do {
        return try exprMacro.expansion(of: parentExpansion, in: &context)
      } catch {
        // Record the error
        context.diagnose(
          Diagnostic(
            node: Syntax(parentExpansion),
            message: ThrownErrorDiagnostic(message: String(describing: error))
          )
        )

        return ExprSyntax(parentExpansion)
      }
    }

    // Emit diagnostics accumulated in the context.
    let macroName = parentExpansion.macro.withoutTrivia().description
    for diag in context.diagnostics {
      emitDiagnostic(
        diagEnginePtr: diagEnginePtr,
        sourceFileBuffer: .init(mutating: sourceFile.pointee.buffer),
        diagnostic: diag,
        messageSuffix: " (from macro '\(macroName)')"
      )
    }

    var evaluatedSyntaxStr = evaluatedSyntax.withoutTrivia().description
    evaluatedSyntaxStr.withUTF8 { utf8 in
      let evaluatedResultPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: utf8.count + 1)
      if let baseAddress = utf8.baseAddress {
        evaluatedResultPtr.initialize(from: baseAddress, count: utf8.count)
      }
      evaluatedResultPtr[utf8.count] = 0

      expandedSourcePointer.pointee = UnsafePointer(evaluatedResultPtr)
      expandedSourceLength.pointee = utf8.count
    }

    return 0
  }
}
