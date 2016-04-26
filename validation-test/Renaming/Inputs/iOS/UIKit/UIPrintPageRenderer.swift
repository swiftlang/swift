
@available(iOS 4.2, *)
class UIPrintPageRenderer : NSObject {
  var headerHeight: CGFloat
  var footerHeight: CGFloat
  var paperRect: CGRect { get }
  var printableRect: CGRect { get }
  var printFormatters: [UIPrintFormatter]?
  @discardableResult
  func printFormattersForPage(at pageIndex: Int) -> [UIPrintFormatter]?
  func addPrintFormatter(_ formatter: UIPrintFormatter, startingAtPageAt pageIndex: Int)
  @discardableResult
  func numberOfPages() -> Int
  func prepare(forDrawingPages range: NSRange)
  func drawPage(at pageIndex: Int, in printableRect: CGRect)
  func drawPrintFormatter(_ printFormatter: UIPrintFormatter, forPageAt pageIndex: Int)
  func drawHeaderForPage(at pageIndex: Int, in headerRect: CGRect)
  func drawContentForPage(at pageIndex: Int, in contentRect: CGRect)
  func drawFooterForPage(at pageIndex: Int, in footerRect: CGRect)
}
