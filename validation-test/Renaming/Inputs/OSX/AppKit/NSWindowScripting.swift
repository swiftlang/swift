
extension NSWindow {
  var hasCloseBox: Bool { get }
  var hasTitleBar: Bool { get }
  var isFloatingPanel: Bool { get }
  var isMiniaturizable: Bool { get }
  var isModalPanel: Bool { get }
  var isResizable: Bool { get }
  var isZoomable: Bool { get }
  var orderedIndex: Int
  func setIsMiniaturized(_ flag: Bool)
  func setIsVisible(_ flag: Bool)
  func setIsZoomed(_ flag: Bool)
  @discardableResult
  func handleCloseScriptCommand(_ command: NSCloseCommand) -> AnyObject?
  @discardableResult
  func handlePrint(_ command: NSScriptCommand) -> AnyObject?
  @discardableResult
  func handleSave(_ command: NSScriptCommand) -> AnyObject?
}
