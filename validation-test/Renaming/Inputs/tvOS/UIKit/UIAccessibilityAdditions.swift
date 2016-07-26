
protocol UIScrollViewAccessibilityDelegate : UIScrollViewDelegate {
  @available(tvOS 2.0, *)
  @discardableResult
  optional func accessibilityScrollStatus(for scrollView: UIScrollView) -> String?
}
