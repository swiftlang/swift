
@available(iOS 7.0, *)
enum UIInputViewStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case keyboard
}
@available(iOS 7.0, *)
class UIInputView : UIView {
  var inputViewStyle: UIInputViewStyle { get }
  @available(iOS 9.0, *)
  var allowsSelfSizing: Bool
  init(frame frame: CGRect, inputViewStyle inputViewStyle: UIInputViewStyle)
}
