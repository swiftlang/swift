
protocol CIFilterConstructor {
  @available(iOS 5.0, *)
  @discardableResult
  func filter(withName name: String) -> CIFilter?
}
