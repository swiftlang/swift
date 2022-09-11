@_cdecl("parseTopLevelSwift")
public func parseTopLevelSwift(
    buffer: UnsafePointer<CChar>, declContext: UnsafeMutableRawPointer,
    ctx: UnsafeMutableRawPointer,
    outputContext: UnsafeMutableRawPointer,
    callback: @convention(c) (UnsafeMutableRawPointer, UnsafeMutableRawPointer) -> Void
) {
  fatalError("Please run build script with --early-swiftsyntax to use ASTGen.")
}