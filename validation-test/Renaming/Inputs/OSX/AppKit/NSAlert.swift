
enum NSAlertStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case warningAlertStyle
  case informationalAlertStyle
  case criticalAlertStyle
}
class NSAlert : NSObject {
  /*not inherited*/ init(error error: NSError)
  var messageText: String
  var informativeText: String
  var icon: NSImage!
  @discardableResult
  func addButton(withTitle title: String) -> NSButton
  var buttons: [NSButton] { get }
  var showsHelp: Bool
  var helpAnchor: String?
  var alertStyle: NSAlertStyle
  unowned(unsafe) var delegate: @sil_unmanaged NSAlertDelegate?
  @available(OSX 10.5, *)
  var showsSuppressionButton: Bool
  @available(OSX 10.5, *)
  var suppressionButton: NSButton? { get }
  @available(OSX 10.5, *)
  var accessoryView: NSView?
  @available(OSX 10.5, *)
  func layout()
  @discardableResult
  func runModal() -> NSModalResponse
  @available(OSX, introduced: 10.3, deprecated: 10.10, message: "Use -beginSheetModalForWindow:completionHandler: instead")
  func beginSheetModal(for window: NSWindow, modalDelegate delegate: AnyObject?, didEnd didEndSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.9, *)
  func beginSheetModal(for sheetWindow: NSWindow, completionHandler handler: ((NSModalResponse) -> Void)? = nil)
  var window: NSWindow { get }
}
var NSAlertFirstButtonReturn: Int { get }
var NSAlertSecondButtonReturn: Int { get }
var NSAlertThirdButtonReturn: Int { get }
protocol NSAlertDelegate : NSObjectProtocol {
  @discardableResult
  optional func alertShowHelp(_ alert: NSAlert) -> Bool
}
