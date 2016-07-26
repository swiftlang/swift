
protocol NSWindowRestoration : NSObjectProtocol {
  @available(OSX 10.7, *)
  static func restoreWindow(withIdentifier identifier: String, state state: NSCoder, completionHandler completionHandler: (NSWindow?, NSError?) -> Void)
}
extension NSDocumentController : NSWindowRestoration {
}
extension NSApplication {
  @available(OSX 10.7, *)
  @discardableResult
  func restoreWindow(withIdentifier identifier: String, state state: NSCoder, completionHandler completionHandler: (NSWindow?, NSError?) -> Void) -> Bool
}
@available(OSX 10.7, *)
let NSApplicationDidFinishRestoringWindowsNotification: String
extension NSWindow {
  @available(OSX 10.7, *)
  var isRestorable: Bool
  @available(OSX 10.7, *)
  var restorationClass: AnyObject.Type?
  func disableSnapshotRestoration()
  func enableSnapshotRestoration()
}
extension NSResponder {
  @available(OSX 10.7, *)
  func encodeRestorableState(with coder: NSCoder)
  @available(OSX 10.7, *)
  func restoreState(with coder: NSCoder)
  @available(OSX 10.7, *)
  func invalidateRestorableState()
  @available(OSX 10.7, *)
  @discardableResult
  class func restorableStateKeyPaths() -> [String]
}
extension NSApplication {
  @available(OSX 10.7, *)
  func extendStateRestoration()
  @available(OSX 10.7, *)
  func completeStateRestoration()
}
extension NSDocument {
  @available(OSX 10.7, *)
  func restoreWindow(withIdentifier identifier: String, state state: NSCoder, completionHandler completionHandler: (NSWindow?, NSError?) -> Void)
  @available(OSX 10.7, *)
  func encodeRestorableState(with coder: NSCoder)
  @available(OSX 10.7, *)
  func restoreState(with coder: NSCoder)
  @available(OSX 10.7, *)
  func invalidateRestorableState()
  @available(OSX 10.7, *)
  @discardableResult
  class func restorableStateKeyPaths() -> [String]
}
