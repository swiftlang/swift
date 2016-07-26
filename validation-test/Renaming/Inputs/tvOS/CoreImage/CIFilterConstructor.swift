
protocol CIFilterConstructor {
  @available(tvOS 5.0, *)
  @discardableResult
  func filter(withName name: String) -> CIFilter?
}
