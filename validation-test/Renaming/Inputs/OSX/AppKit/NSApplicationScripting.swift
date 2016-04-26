
extension NSApplication {
  var orderedDocuments: [NSDocument] { get }
  var orderedWindows: [NSWindow] { get }
}
extension NSObject {
  @discardableResult
  class func application(_ sender: NSApplication, delegateHandlesKey key: String) -> Bool
  @discardableResult
  func application(_ sender: NSApplication, delegateHandlesKey key: String) -> Bool
}
