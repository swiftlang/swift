
@available(iOS 2.0, *)
class UIResponder : NSObject {
  @discardableResult
  func next() -> UIResponder?
  @discardableResult
  func canBecomeFirstResponder() -> Bool
  @discardableResult
  func becomeFirstResponder() -> Bool
  @discardableResult
  func canResignFirstResponder() -> Bool
  @discardableResult
  func resignFirstResponder() -> Bool
  @discardableResult
  func isFirstResponder() -> Bool
  func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?)
  func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?)
  func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?)
  func touchesCancelled(_ touches: Set<UITouch>?, with event: UIEvent?)
  @available(iOS 9.1, *)
  func touchesEstimatedPropertiesUpdated(_ touches: Set<NSObject>)
  @available(iOS 9.0, *)
  func pressesBegan(_ presses: Set<UIPress>, with event: UIPressesEvent?)
  @available(iOS 9.0, *)
  func pressesChanged(_ presses: Set<UIPress>, with event: UIPressesEvent?)
  @available(iOS 9.0, *)
  func pressesEnded(_ presses: Set<UIPress>, with event: UIPressesEvent?)
  @available(iOS 9.0, *)
  func pressesCancelled(_ presses: Set<UIPress>, with event: UIPressesEvent?)
  @available(iOS 3.0, *)
  func motionBegan(_ motion: UIEventSubtype, with event: UIEvent?)
  @available(iOS 3.0, *)
  func motionEnded(_ motion: UIEventSubtype, with event: UIEvent?)
  @available(iOS 3.0, *)
  func motionCancelled(_ motion: UIEventSubtype, with event: UIEvent?)
  @available(iOS 4.0, *)
  func remoteControlReceived(with event: UIEvent?)
  @available(iOS 3.0, *)
  @discardableResult
  func canPerformAction(_ action: Selector, withSender sender: AnyObject?) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  func target(forAction action: Selector, withSender sender: AnyObject?) -> AnyObject?
  @available(iOS 3.0, *)
  var undoManager: NSUndoManager? { get }
}
@available(iOS 7.0, *)
struct UIKeyModifierFlags : OptionSet {
  init(rawValue rawValue: Int)
  let rawValue: Int
  static var alphaShift: UIKeyModifierFlags { get }
  static var shift: UIKeyModifierFlags { get }
  static var control: UIKeyModifierFlags { get }
  static var alternate: UIKeyModifierFlags { get }
  static var command: UIKeyModifierFlags { get }
  static var numericPad: UIKeyModifierFlags { get }
}
@available(iOS 7.0, *)
class UIKeyCommand : NSObject, NSCopying, NSSecureCoding {
  var input: String { get }
  var modifierFlags: UIKeyModifierFlags { get }
  @available(iOS 9.0, *)
  var discoverabilityTitle: String?
  /*not inherited*/ init(input input: String, modifierFlags modifierFlags: UIKeyModifierFlags, action action: Selector)
  @available(iOS 9.0, *)
  /*not inherited*/ init(input input: String, modifierFlags modifierFlags: UIKeyModifierFlags, action action: Selector, discoverabilityTitle discoverabilityTitle: String)
}
extension UIResponder {
  @available(iOS 7.0, *)
  var keyCommands: [UIKeyCommand]? { get }
}
extension NSObject {
  @available(iOS 3.0, *)
  class func cut(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  func cut(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  class func copy(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  func copy(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  class func paste(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  func paste(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  class func select(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  func select(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  class func selectAll(_ sender: AnyObject?)
  @available(iOS 3.0, *)
  func selectAll(_ sender: AnyObject?)
  @available(iOS 3.2, *)
  class func delete(_ sender: AnyObject?)
  @available(iOS 3.2, *)
  func delete(_ sender: AnyObject?)
  @available(iOS 5.0, *)
  class func makeTextWritingDirectionLeftToRight(_ sender: AnyObject?)
  @available(iOS 5.0, *)
  func makeTextWritingDirectionLeftToRight(_ sender: AnyObject?)
  @available(iOS 5.0, *)
  class func makeTextWritingDirectionRightToLeft(_ sender: AnyObject?)
  @available(iOS 5.0, *)
  func makeTextWritingDirectionRightToLeft(_ sender: AnyObject?)
  @available(iOS 6.0, *)
  class func toggleBoldface(_ sender: AnyObject?)
  @available(iOS 6.0, *)
  func toggleBoldface(_ sender: AnyObject?)
  @available(iOS 6.0, *)
  class func toggleItalics(_ sender: AnyObject?)
  @available(iOS 6.0, *)
  func toggleItalics(_ sender: AnyObject?)
  @available(iOS 6.0, *)
  class func toggleUnderline(_ sender: AnyObject?)
  @available(iOS 6.0, *)
  func toggleUnderline(_ sender: AnyObject?)
  @available(iOS 7.0, *)
  class func increaseSize(_ sender: AnyObject?)
  @available(iOS 7.0, *)
  func increaseSize(_ sender: AnyObject?)
  @available(iOS 7.0, *)
  class func decreaseSize(_ sender: AnyObject?)
  @available(iOS 7.0, *)
  func decreaseSize(_ sender: AnyObject?)
}
extension UIResponder {
  @available(iOS 3.2, *)
  var inputView: UIView? { get }
  @available(iOS 3.2, *)
  var inputAccessoryView: UIView? { get }
  @available(iOS 9.0, *)
  var inputAssistantItem: UITextInputAssistantItem { get }
  @available(iOS 8.0, *)
  var inputViewController: UIInputViewController? { get }
  @available(iOS 8.0, *)
  var inputAccessoryViewController: UIInputViewController? { get }
  @available(iOS 7.0, *)
  var textInputMode: UITextInputMode? { get }
  @available(iOS 7.0, *)
  var textInputContextIdentifier: String? { get }
  @available(iOS 7.0, *)
  class func clearTextInputContextIdentifier(_ identifier: String)
  @available(iOS 3.2, *)
  func reloadInputViews()
}
@available(iOS 7.0, *)
let UIKeyInputUpArrow: String
@available(iOS 7.0, *)
let UIKeyInputDownArrow: String
@available(iOS 7.0, *)
let UIKeyInputLeftArrow: String
@available(iOS 7.0, *)
let UIKeyInputRightArrow: String
@available(iOS 7.0, *)
let UIKeyInputEscape: String
extension UIResponder {
  @available(iOS 8.0, *)
  var userActivity: NSUserActivity?
  @available(iOS 8.0, *)
  func updateUserActivityState(_ activity: NSUserActivity)
  @available(iOS 8.0, *)
  func restoreUserActivityState(_ activity: NSUserActivity)
}
