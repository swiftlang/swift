
typealias UIPrintInteractionCompletionHandler = (UIPrintInteractionController, Bool, NSError?) -> Void
@available(iOS 9.0, *)
enum UIPrinterCutterBehavior : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case noCut
  case printerDefault
  case cutAfterEachPage
  case cutAfterEachCopy
  case cutAfterEachJob
}
@available(iOS 4.2, *)
class UIPrintInteractionController : NSObject {
  @discardableResult
  class func isPrintingAvailable() -> Bool
  @discardableResult
  class func printableUTIs() -> Set<String>
  @discardableResult
  class func canPrint(_ url: NSURL) -> Bool
  @discardableResult
  class func canPrint(_ data: NSData) -> Bool
  @discardableResult
  class func sharedPrint() -> UIPrintInteractionController
  var printInfo: UIPrintInfo?
  weak var delegate: @sil_weak UIPrintInteractionControllerDelegate?
  var showsPageRange: Bool
  @available(iOS 7.0, *)
  var showsNumberOfCopies: Bool
  @available(iOS 8.0, *)
  var showsPaperSelectionForLoadedPapers: Bool
  var printPaper: UIPrintPaper? { get }
  var printPageRenderer: UIPrintPageRenderer?
  var printFormatter: UIPrintFormatter?
  @NSCopying var printingItem: AnyObject?
  var printingItems: [AnyObject]?
  @discardableResult
  func present(animated animated: Bool, completionHandler completion: UIPrintInteractionCompletionHandler? = nil) -> Bool
  @discardableResult
  func present(from rect: CGRect, in view: UIView, animated animated: Bool, completionHandler completion: UIPrintInteractionCompletionHandler? = nil) -> Bool
  @discardableResult
  func present(from item: UIBarButtonItem, animated animated: Bool, completionHandler completion: UIPrintInteractionCompletionHandler? = nil) -> Bool
  @discardableResult
  func print(to printer: UIPrinter, completionHandler completion: UIPrintInteractionCompletionHandler? = nil) -> Bool
  func dismiss(animated animated: Bool)
}
protocol UIPrintInteractionControllerDelegate : NSObjectProtocol {
  @available(iOS 4.2, *)
  @discardableResult
  optional func printInteractionControllerParentViewController(_ printInteractionController: UIPrintInteractionController) -> UIViewController
  @available(iOS 4.2, *)
  @discardableResult
  optional func printInteractionController(_ printInteractionController: UIPrintInteractionController, choosePaper paperList: [UIPrintPaper]) -> UIPrintPaper
  @available(iOS 4.2, *)
  optional func printInteractionControllerWillPresentPrinterOptions(_ printInteractionController: UIPrintInteractionController)
  @available(iOS 4.2, *)
  optional func printInteractionControllerDidPresentPrinterOptions(_ printInteractionController: UIPrintInteractionController)
  @available(iOS 4.2, *)
  optional func printInteractionControllerWillDismissPrinterOptions(_ printInteractionController: UIPrintInteractionController)
  @available(iOS 4.2, *)
  optional func printInteractionControllerDidDismissPrinterOptions(_ printInteractionController: UIPrintInteractionController)
  @available(iOS 4.2, *)
  optional func printInteractionControllerWillStartJob(_ printInteractionController: UIPrintInteractionController)
  @available(iOS 4.2, *)
  optional func printInteractionControllerDidFinishJob(_ printInteractionController: UIPrintInteractionController)
  @available(iOS 7.0, *)
  @discardableResult
  optional func printInteractionController(_ printInteractionController: UIPrintInteractionController, cutLengthFor paper: UIPrintPaper) -> CGFloat
  @available(iOS 9.0, *)
  @discardableResult
  optional func printInteractionController(_ printInteractionController: UIPrintInteractionController, chooseCutterBehavior availableBehaviors: [AnyObject]) -> UIPrinterCutterBehavior
}
