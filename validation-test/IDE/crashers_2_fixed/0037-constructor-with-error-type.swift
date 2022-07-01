public struct AnyError {

  public init(_ error: Swift.

extension AnyError {
  public static func error(from error: Error) -> AnyError {
    return AnyError(error)#^COMPLETE^#
  }
}
