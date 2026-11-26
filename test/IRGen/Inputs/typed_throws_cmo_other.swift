public struct RenderStore {
  public var data: [UInt8]
  public init() { data = [] }
}

public struct ConvertFailure: Error {
  public init() {}
}

public final class Server {
  public init() {}
  public func convert(_ b: Bool, _ n: Int) async throws -> RenderStore {
    if b && n > 3 { throw ConvertFailure() }
    return RenderStore()
  }
}
