import SwiftParser
import SwiftSyntax

/// Describes a source file that has been "exported" to the C++ part of the
/// compiler, with enough information to interface with the C++ layer.
struct ExportedSourceFile {
  /// The underlying buffer within the C++ SourceManager, which is used
  /// for computations of source locations.
  let buffer: UnsafeBufferPointer<UInt8>

  /// The name of the enclosing module.
  let moduleName: String

  /// The name of the source file being parsed.
  let fileName: String

  /// The syntax tree for the complete source file.
  let syntax: SourceFileSyntax
}

/// Parses the given source file and produces a pointer to a new
/// ExportedSourceFile instance.
@_cdecl("swift_ASTGen_parseSourceFile")
public func parseSourceFile(
  buffer: UnsafePointer<UInt8>, bufferLength: Int,
  moduleName: UnsafePointer<UInt8>, filename: UnsafePointer<UInt8>
) -> UnsafeRawPointer {
  let buffer = UnsafeBufferPointer(start: buffer, count: bufferLength)
  let sourceFile = Parser.parse(source: buffer)

  let exportedPtr = UnsafeMutablePointer<ExportedSourceFile>.allocate(capacity: 1)
  exportedPtr.initialize(to: .init(
    buffer: buffer, moduleName: String(cString: moduleName),
    fileName: String(cString: filename), syntax: sourceFile)
  )

  return UnsafeRawPointer(exportedPtr)
}

/// Deallocate a parsed source file.
@_cdecl("swift_ASTGen_destroySourceFile")
public func destroySourceFile(
  sourceFilePtr: UnsafeMutablePointer<UInt8>
) {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sourceFile in
    sourceFile.deinitialize(count: 1)
    sourceFile.deallocate()
  }
}
