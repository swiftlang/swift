
class NSHelpManager : NSObject {
  @discardableResult
  class func shared() -> NSHelpManager
  class func setContextHelpModeActive(_ active: Bool)
  @discardableResult
  class func isContextHelpModeActive() -> Bool
  func setContextHelp(_ attrString: NSAttributedString, for object: AnyObject)
  func removeContextHelp(for object: AnyObject)
  @discardableResult
  func contextHelp(for object: AnyObject) -> NSAttributedString?
  @discardableResult
  func showContextHelp(for object: AnyObject, locationHint pt: NSPoint) -> Bool
  func openHelpAnchor(_ anchor: String, inBook book: String?)
  func find(_ query: String, inBook book: String?)
  @available(OSX 10.6, *)
  @discardableResult
  func registerBooks(in bundle: NSBundle) -> Bool
}
let NSContextHelpModeDidActivateNotification: String
let NSContextHelpModeDidDeactivateNotification: String
extension NSBundle {
  @discardableResult
  func contextHelp(forKey key: String) -> NSAttributedString?
}
extension NSApplication {
  func activateContextHelpMode(_ sender: AnyObject?)
  func showHelp(_ sender: AnyObject?)
}
