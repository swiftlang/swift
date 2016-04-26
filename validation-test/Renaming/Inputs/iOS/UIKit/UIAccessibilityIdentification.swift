
protocol UIAccessibilityIdentification : NSObjectProtocol {
  @available(iOS 5.0, *)
  var accessibilityIdentifier: String? { get set }
}
extension UIView : UIAccessibilityIdentification {
}
extension UIBarItem : UIAccessibilityIdentification {
}
extension UIImage : UIAccessibilityIdentification {
}
