
protocol NSExtensionRequestHandling : NSObjectProtocol {
  @available(OSX 10.10, *)
  func beginRequest(with context: NSExtensionContext)
}
