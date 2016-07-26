
extension NSDocument {
  var lastComponentOfFileName: String
  @discardableResult
  func handleSave(_ command: NSScriptCommand) -> AnyObject?
  @discardableResult
  func handleCloseScriptCommand(_ command: NSCloseCommand) -> AnyObject?
  @discardableResult
  func handlePrint(_ command: NSScriptCommand) -> AnyObject?
}
