
@available(iOS 4.2, *)
class UIPrintFormatter : NSObject, NSCopying {
  weak var printPageRenderer: @sil_weak UIPrintPageRenderer? { get }
  func removeFromPrintPageRenderer()
  var maximumContentHeight: CGFloat
  var maximumContentWidth: CGFloat
  var contentInsets: UIEdgeInsets
  var perPageContentInsets: UIEdgeInsets
  var startPage: Int
  var pageCount: Int { get }
  @discardableResult
  func rectForPage(at pageIndex: Int) -> CGRect
  func draw(in rect: CGRect, forPageAt pageIndex: Int)
}
@available(iOS 4.2, *)
class UISimpleTextPrintFormatter : UIPrintFormatter {
  init(text text: String)
  @available(iOS 7.0, *)
  init(attributedText attributedText: NSAttributedString)
  var text: String?
  @available(iOS 7.0, *)
  @NSCopying var attributedText: NSAttributedString?
  var font: UIFont?
  var color: UIColor?
  var textAlignment: NSTextAlignment
}
@available(iOS 4.2, *)
class UIMarkupTextPrintFormatter : UIPrintFormatter {
  init(markupText markupText: String)
  var markupText: String?
}
@available(iOS 4.2, *)
class UIViewPrintFormatter : UIPrintFormatter {
  var view: UIView { get }
}
extension UIView {
  @discardableResult
  func viewPrintFormatter() -> UIViewPrintFormatter
  func draw(_ rect: CGRect, for formatter: UIViewPrintFormatter)
}
