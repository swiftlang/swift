public struct MyDouble {
  @available(swift, deprecated: 3.1, obsoleted: 4.0, message: "Please use the `abs(_:)` free function")
  public static func abs(_ x: Double) -> Double {
    return x
  }
}
