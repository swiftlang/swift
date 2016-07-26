
enum UITextBorderStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case line
  case bezel
  case roundedRect
}
enum UITextFieldViewMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case never
  case whileEditing
  case unlessEditing
  case always
}
@available(tvOS 2.0, *)
class UITextField : UIControl, UITextInput, NSCoding {
  var text: String?
  @available(tvOS 6.0, *)
  @NSCopying var attributedText: NSAttributedString?
  var textColor: UIColor?
  var font: UIFont?
  var textAlignment: NSTextAlignment
  var borderStyle: UITextBorderStyle
  @available(tvOS 7.0, *)
  var defaultTextAttributes: [String : AnyObject]
  var placeholder: String?
  @available(tvOS 6.0, *)
  @NSCopying var attributedPlaceholder: NSAttributedString?
  var clearsOnBeginEditing: Bool
  var adjustsFontSizeToFitWidth: Bool
  var minimumFontSize: CGFloat
  weak var delegate: @sil_weak UITextFieldDelegate?
  var background: UIImage?
  var disabledBackground: UIImage?
  var isEditing: Bool { get }
  @available(tvOS 6.0, *)
  var allowsEditingTextAttributes: Bool
  @available(tvOS 6.0, *)
  var typingAttributes: [String : AnyObject]?
  var clearButtonMode: UITextFieldViewMode
  var leftView: UIView?
  var leftViewMode: UITextFieldViewMode
  var rightView: UIView?
  var rightViewMode: UITextFieldViewMode
  @discardableResult
  func borderRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func textRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func placeholderRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func editingRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func clearButtonRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func leftViewRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func rightViewRect(forBounds bounds: CGRect) -> CGRect
  func drawText(in rect: CGRect)
  func drawPlaceholder(in rect: CGRect)
  @available(tvOS 6.0, *)
  var clearsOnInsertion: Bool
}
extension UIView {
  @discardableResult
  func endEditing(_ force: Bool) -> Bool
}
protocol UITextFieldDelegate : NSObjectProtocol {
  @available(tvOS 2.0, *)
  @discardableResult
  optional func textFieldShouldBeginEditing(_ textField: UITextField) -> Bool
  @available(tvOS 2.0, *)
  optional func textFieldDidBeginEditing(_ textField: UITextField)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func textFieldShouldEndEditing(_ textField: UITextField) -> Bool
  @available(tvOS 2.0, *)
  optional func textFieldDidEndEditing(_ textField: UITextField)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func textField(_ textField: UITextField, shouldChangeCharactersIn range: NSRange, replacementString string: String) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  optional func textFieldShouldClear(_ textField: UITextField) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  optional func textFieldShouldReturn(_ textField: UITextField) -> Bool
}
let UITextFieldTextDidBeginEditingNotification: String
let UITextFieldTextDidEndEditingNotification: String
let UITextFieldTextDidChangeNotification: String
