import CASTBridging
import SwiftDiagnostics
import SwiftSyntax

/// A set of queued diagnostics created by the C++ compiler and rendered
/// via the swift-syntax renderer.
struct QueuedDiagnostics {
  /// The source file in which all of the diagnostics occur.
  let sourceFile: SourceFileSyntax

  /// The underlying buffer within the C++ SourceManager, which is used
  /// for computations of source locations.
  let buffer: UnsafeBufferPointer<UInt8>

  /// The set of diagnostics.
  fileprivate var diagnostics: [Diagnostic] = []

  mutating func diagnose(_ diagnostic: Diagnostic) {
    assert(diagnostic.node.root == sourceFile.root)
    diagnostics.append(diagnostic)
  }

  func render() -> String {
    return DiagnosticsFormatter.annotatedSource(
      tree: sourceFile,
      diags: diagnostics
    )
  }
}

/// Create a queued diagnostics structure in which we can
@_cdecl("swift_ASTGen_createQueuedDiagnostics")
public func createQueuedDiagnostics(
  sourceFilePtr: UnsafeMutablePointer<UInt8>
) -> UnsafeRawPointer {
  return sourceFilePtr.withMemoryRebound(
    to: ExportedSourceFile.self, capacity: 1
  ) { sourceFile in
    let ptr = UnsafeMutablePointer<QueuedDiagnostics>.allocate(capacity: 1)
    ptr.initialize(to: .init(
      sourceFile: sourceFile.pointee.syntax,
      buffer: sourceFile.pointee.buffer)
    )
    return UnsafeRawPointer(ptr)
  }
}

/// Destroy the queued diagnostics.
@_cdecl("swift_ASTGen_destroyQueuedDiagnostics")
public func destroyQueuedDiagnostics(
  queuedDiagnosticsPtr: UnsafeMutablePointer<UInt8>
) {
  queuedDiagnosticsPtr.withMemoryRebound(to: QueuedDiagnostics.self, capacity: 1) { queuedDiagnostics in
    queuedDiagnostics.deinitialize(count: 1)
    queuedDiagnostics.deallocate()
  }
}

/// Diagnostic message used for thrown errors.
fileprivate struct SimpleDiagnostic: DiagnosticMessage {
  let message: String

  let severity: DiagnosticSeverity

  var diagnosticID: MessageID {
    .init(domain: "SwiftCompiler", id: "SimpleDiagnostic")
  }
}

extension BridgedDiagnosticSeverity {
  var asSeverity: DiagnosticSeverity {
    switch self {
    case .fatalError: return .error
    case .error: return .error
    case .warning: return .warning
    case .remark: return .warning // FIXME
    case .note: return .note
    @unknown default: return .error
    }
  }
}

/// Add a new diagnostic to the queue.
@_cdecl("swift_ASTGen_addQueuedDiagnostic")
public func addQueuedDiagnostic(
  queuedDiagnosticsPtr: UnsafeMutableRawPointer,
  text: UnsafePointer<UInt8>,
  textLength: Int,
  severity: BridgedDiagnosticSeverity,
  position: UnsafePointer<UInt8>
) {
  let queuedDiagnostics = queuedDiagnosticsPtr.bindMemory(
    to: QueuedDiagnostics.self, capacity: 1
  )

  // Find the offset.
  let buffer = queuedDiagnostics.pointee.buffer
  let offset = position - buffer.baseAddress!
  if offset < 0 || offset >= buffer.count {
    return
  }

  // Find the token at that offset.
  let sf = queuedDiagnostics.pointee.sourceFile
  guard let token = sf.token(at: AbsolutePosition(utf8Offset: offset)) else {
    return
  }

  let textBuffer = UnsafeBufferPointer(start: text, count: textLength)
  let diagnostic = Diagnostic(
    node: Syntax(token),
    message: SimpleDiagnostic(
      message: String(decoding: textBuffer, as: UTF8.self),
      severity: severity.asSeverity
    )
  )

  queuedDiagnostics.pointee.diagnose(diagnostic)
}


/// Render the queued diagnostics into a UTF-8 string.
@_cdecl("swift_ASTGen_renderQueuedDiagnostics")
public func renterQueuedDiagnostics(
  queuedDiagnosticsPtr: UnsafeMutablePointer<UInt8>,
  contextSize: Int,
  colorize: Int,
  renderedPointer: UnsafeMutablePointer<UnsafePointer<UInt8>?>,
  renderedLength: UnsafeMutablePointer<Int>
) {
  queuedDiagnosticsPtr.withMemoryRebound(to: QueuedDiagnostics.self, capacity: 1) { queuedDiagnostics in
    let renderedStr = DiagnosticsFormatter.annotatedSource(
      tree: queuedDiagnostics.pointee.sourceFile,
      diags: queuedDiagnostics.pointee.diagnostics,
      contextSize: contextSize,
      colorize: colorize != 0
    )

    (renderedPointer.pointee, renderedLength.pointee) =
        allocateUTF8String(renderedStr)
  }
}
