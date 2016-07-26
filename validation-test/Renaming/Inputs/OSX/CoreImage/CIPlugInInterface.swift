
protocol CIPlugInRegistration {
  @discardableResult
  func load(_ host: UnsafeMutablePointer<Void>!) -> Bool
}
