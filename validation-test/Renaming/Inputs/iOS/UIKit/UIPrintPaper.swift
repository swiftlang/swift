
@available(iOS 4.2, *)
class UIPrintPaper : NSObject {
  @discardableResult
  class func bestPaper(forPageSize contentSize: CGSize, withPapersFrom paperList: [UIPrintPaper]) -> UIPrintPaper
  var paperSize: CGSize { get }
  var printableRect: CGRect { get }
}
extension UIPrintPaper {
  @discardableResult
  func printRect() -> CGRect
}
