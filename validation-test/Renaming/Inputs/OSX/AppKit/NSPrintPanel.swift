
@available(OSX 10.5, *)
struct NSPrintPanelOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var showsCopies: NSPrintPanelOptions { get }
  static var showsPageRange: NSPrintPanelOptions { get }
  static var showsPaperSize: NSPrintPanelOptions { get }
  static var showsOrientation: NSPrintPanelOptions { get }
  static var showsScaling: NSPrintPanelOptions { get }
  @available(OSX 10.6, *)
  static var showsPrintSelection: NSPrintPanelOptions { get }
  static var showsPageSetupAccessory: NSPrintPanelOptions { get }
  static var showsPreview: NSPrintPanelOptions { get }
}
let NSPrintPhotoJobStyleHint: String
@available(OSX 10.6, *)
let NSPrintAllPresetsJobStyleHint: String
@available(OSX 10.6, *)
let NSPrintNoPresetsJobStyleHint: String
@available(OSX 10.5, *)
let NSPrintPanelAccessorySummaryItemNameKey: String
@available(OSX 10.5, *)
let NSPrintPanelAccessorySummaryItemDescriptionKey: String
protocol NSPrintPanelAccessorizing {
  @discardableResult
  func localizedSummaryItems() -> [[String : String]]
  @discardableResult
  optional func keyPathsForValuesAffectingPreview() -> Set<String>
}
class NSPrintPanel : NSObject {
  @available(OSX 10.5, *)
  func addAccessoryController(_ accessoryController: NSViewController)
  @available(OSX 10.5, *)
  func removeAccessoryController(_ accessoryController: NSViewController)
  @available(OSX 10.5, *)
  var accessoryControllers: [NSViewController] { get }
  @available(OSX 10.5, *)
  var options: NSPrintPanelOptions
  @available(OSX 10.5, *)
  func setDefaultButtonTitle(_ defaultButtonTitle: String?)
  @available(OSX 10.5, *)
  @discardableResult
  func defaultButtonTitle() -> String?
  @available(OSX 10.5, *)
  var helpAnchor: String?
  var jobStyleHint: String?
  func beginSheet(with printInfo: NSPrintInfo, modalFor docWindow: NSWindow, delegate delegate: AnyObject?, didEnd didEndSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.5, *)
  @discardableResult
  func runModal(with printInfo: NSPrintInfo) -> Int
  @discardableResult
  func runModal() -> Int
  @available(OSX 10.5, *)
  var printInfo: NSPrintInfo { get }
}
extension NSPrintPanel {
}
