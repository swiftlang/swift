
protocol UIPickerViewAccessibilityDelegate : UIPickerViewDelegate {
  @available(iOS 2.0, *)
  @discardableResult
  optional func pickerView(_ pickerView: UIPickerView, accessibilityLabelForComponent component: Int) -> String?
  @available(iOS 2.0, *)
  @discardableResult
  optional func pickerView(_ pickerView: UIPickerView, accessibilityHintForComponent component: Int) -> String?
}
protocol UIScrollViewAccessibilityDelegate : UIScrollViewDelegate {
  @available(iOS 2.0, *)
  @discardableResult
  optional func accessibilityScrollStatus(for scrollView: UIScrollView) -> String?
}
