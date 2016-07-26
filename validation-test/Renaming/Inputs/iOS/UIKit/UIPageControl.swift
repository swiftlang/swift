
@available(iOS 2.0, *)
class UIPageControl : UIControl {
  var numberOfPages: Int
  var currentPage: Int
  var hidesForSinglePage: Bool
  var defersCurrentPageDisplay: Bool
  func updateCurrentPageDisplay()
  @discardableResult
  func sizeForNumber(ofPages pageCount: Int) -> CGSize
  @available(iOS 6.0, *)
  var pageIndicatorTintColor: UIColor?
  @available(iOS 6.0, *)
  var currentPageIndicatorTintColor: UIColor?
}
