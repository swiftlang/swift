public protocol OtherOS {
  var name: String { get }
}

public struct Linux: OtherOS {
  public let name = "Linux"
}
