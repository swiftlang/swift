
protocol CIFilterConstructor {
  @available(OSX 10.4, *)
  @discardableResult
  func filter(withName name: String) -> CIFilter?
}
