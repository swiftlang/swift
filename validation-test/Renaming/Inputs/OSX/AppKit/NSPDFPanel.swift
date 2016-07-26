
@available(OSX 10.9, *)
struct NSPDFPanelOptions : OptionSet {
  init(rawValue rawValue: Int)
  let rawValue: Int
  static var showsPaperSize: NSPDFPanelOptions { get }
  static var showsOrientation: NSPDFPanelOptions { get }
  static var requestsParentDirectory: NSPDFPanelOptions { get }
}
@available(OSX 10.9, *)
class NSPDFPanel : NSObject {
  var accessoryController: NSViewController?
  var options: NSPDFPanelOptions
  var defaultFileName: String
  func beginSheet(with pdfInfo: NSPDFInfo, modalFor docWindow: NSWindow?, completionHandler completionHandler: (Int) -> Void)
}
