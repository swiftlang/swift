
enum NSPrintingPageOrder : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case descendingPageOrder
  case specialPageOrder
  case ascendingPageOrder
  case unknownPageOrder
}
@available(OSX 10.7, *)
enum NSPrintRenderingQuality : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case best
  case responsive
}
let NSPrintOperationExistsException: String
class NSPrintOperation : NSObject {
  /*not inherited*/ init(view view: NSView, printInfo printInfo: NSPrintInfo)
  @discardableResult
  class func pdfOperation(with view: NSView, inside rect: NSRect, to data: NSMutableData, printInfo printInfo: NSPrintInfo) -> NSPrintOperation
  @discardableResult
  class func pdfOperation(with view: NSView, inside rect: NSRect, toPath path: String, printInfo printInfo: NSPrintInfo) -> NSPrintOperation
  @discardableResult
  class func epsOperation(with view: NSView, inside rect: NSRect, to data: NSMutableData, printInfo printInfo: NSPrintInfo) -> NSPrintOperation
  @discardableResult
  class func epsOperation(with view: NSView, inside rect: NSRect, toPath path: String, printInfo printInfo: NSPrintInfo) -> NSPrintOperation
  /*not inherited*/ init(view view: NSView)
  @discardableResult
  class func pdfOperation(with view: NSView, inside rect: NSRect, to data: NSMutableData) -> NSPrintOperation
  @discardableResult
  class func epsOperation(with view: NSView, inside rect: NSRect, to data: NSMutableData?) -> NSPrintOperation
  @discardableResult
  class func current() -> NSPrintOperation?
  class func setCurrentOperation(_ operation: NSPrintOperation?)
  var isCopyingOperation: Bool { get }
  @available(OSX 10.7, *)
  var preferredRenderingQuality: NSPrintRenderingQuality { get }
  @available(OSX 10.5, *)
  var jobTitle: String?
  var showsPrintPanel: Bool
  var showsProgressPanel: Bool
  var printPanel: NSPrintPanel
  @available(OSX 10.9, *)
  var pdfPanel: NSPDFPanel
  var canSpawnSeparateThread: Bool
  var pageOrder: NSPrintingPageOrder
  func runModal(for docWindow: NSWindow, delegate delegate: AnyObject?, didRun didRunSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @discardableResult
  func run() -> Bool
  var view: NSView? { get }
  @NSCopying var printInfo: NSPrintInfo
  var context: NSGraphicsContext? { get }
  @available(OSX 10.5, *)
  var pageRange: NSRange { get }
  var currentPage: Int { get }
  @discardableResult
  func createContext() -> NSGraphicsContext?
  func destroyContext()
  @discardableResult
  func deliverResult() -> Bool
  func cleanUp()
}
extension NSPrintOperation {
}
