
@available(OSX 10.9, *)
enum NSPaperOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case portrait
  case landscape
}
enum NSPrintingPaginationMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case autoPagination
  case fitPagination
  case clipPagination
}
let NSPrintSpoolJob: String
let NSPrintPreviewJob: String
let NSPrintSaveJob: String
let NSPrintCancelJob: String
let NSPrintPaperName: String
let NSPrintPaperSize: String
let NSPrintOrientation: String
let NSPrintScalingFactor: String
let NSPrintLeftMargin: String
let NSPrintRightMargin: String
let NSPrintTopMargin: String
let NSPrintBottomMargin: String
let NSPrintHorizontallyCentered: String
let NSPrintVerticallyCentered: String
let NSPrintHorizontalPagination: String
let NSPrintVerticalPagination: String
let NSPrintPrinter: String
let NSPrintCopies: String
let NSPrintAllPages: String
let NSPrintFirstPage: String
let NSPrintLastPage: String
let NSPrintMustCollate: String
let NSPrintReversePageOrder: String
let NSPrintJobDisposition: String
let NSPrintPagesAcross: String
let NSPrintPagesDown: String
let NSPrintTime: String
let NSPrintDetailedErrorReporting: String
let NSPrintFaxNumber: String
let NSPrintPrinterName: String
@available(OSX 10.6, *)
let NSPrintSelectionOnly: String
@available(OSX 10.6, *)
let NSPrintJobSavingURL: String
@available(OSX 10.6, *)
let NSPrintJobSavingFileNameExtensionHidden: String
let NSPrintHeaderAndFooter: String
class NSPrintInfo : NSObject, NSCopying, NSCoding {
  class func setSharedPrintInfo(_ printInfo: NSPrintInfo)
  @discardableResult
  class func shared() -> NSPrintInfo
  init(dictionary attributes: [String : AnyObject])
  @discardableResult
  func dictionary() -> NSMutableDictionary
  var paperName: String?
  var paperSize: NSSize
  var orientation: NSPaperOrientation
  @available(OSX 10.6, *)
  var scalingFactor: CGFloat
  var leftMargin: CGFloat
  var rightMargin: CGFloat
  var topMargin: CGFloat
  var bottomMargin: CGFloat
  var isHorizontallyCentered: Bool
  var isVerticallyCentered: Bool
  var horizontalPagination: NSPrintingPaginationMode
  var verticalPagination: NSPrintingPaginationMode
  var jobDisposition: String
  @NSCopying var printer: NSPrinter
  func setUpPrintOperationDefaultValues()
  var imageablePageBounds: NSRect { get }
  var localizedPaperName: String? { get }
  @discardableResult
  class func defaultPrinter() -> NSPrinter?
  @available(OSX 10.5, *)
  var printSettings: NSMutableDictionary { get }
  @available(OSX 10.5, *)
  @discardableResult
  func pmPrintSession() -> UnsafeMutablePointer<Void>
  @available(OSX 10.5, *)
  @discardableResult
  func pmPageFormat() -> UnsafeMutablePointer<Void>
  @available(OSX 10.5, *)
  @discardableResult
  func pmPrintSettings() -> UnsafeMutablePointer<Void>
  @available(OSX 10.5, *)
  func updateFromPMPageFormat()
  @available(OSX 10.5, *)
  func updateFromPMPrintSettings()
  @available(OSX 10.6, *)
  var isSelectionOnly: Bool
  @available(OSX 10.9, *)
  func takeSettings(from inPDFInfo: NSPDFInfo)
}
extension NSPrintInfo {
}
enum NSPrintingOrientation : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case portraitOrientation
  case landscapeOrientation
}
