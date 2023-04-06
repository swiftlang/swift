// Make sure we don't crash
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

public struct AnyError {

  public init(_ error: Swift.

extension AnyError {
  public static func error(from error: Error) -> AnyError {
    return AnyError(error)#^COMPLETE^#
  }
}
