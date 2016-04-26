
protocol UITextViewDelegate : NSObjectProtocol, UIScrollViewDelegate {
  @available(iOS 2.0, *)
  @discardableResult
  optional func textViewShouldBeginEditing(_ textView: UITextView) -> Bool
  @available(iOS 2.0, *)
  @discardableResult
  optional func textViewShouldEndEditing(_ textView: UITextView) -> Bool
  @available(iOS 2.0, *)
  optional func textViewDidBeginEditing(_ textView: UITextView)
  @available(iOS 2.0, *)
  optional func textViewDidEndEditing(_ textView: UITextView)
  @available(iOS 2.0, *)
  @discardableResult
  optional func textView(_ textView: UITextView, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool
  @available(iOS 2.0, *)
  optional func textViewDidChange(_ textView: UITextView)
  @available(iOS 2.0, *)
  optional func textViewDidChangeSelection(_ textView: UITextView)
  @available(iOS 7.0, *)
  @discardableResult
  optional func textView(_ textView: UITextView, shouldInteractWith URL: NSURL, in characterRange: NSRange) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  optional func textView(_ textView: UITextView, shouldInteractWith textAttachment: NSTextAttachment, in characterRange: NSRange) -> Bool
}
@available(iOS 2.0, *)
class UITextView : UIScrollView, UITextInput {
  var text: String!
  var font: UIFont?
  var textColor: UIColor?
  var textAlignment: NSTextAlignment
  var selectedRange: NSRange
  var isEditable: Bool
  @available(iOS 7.0, *)
  var isSelectable: Bool
  @available(iOS 3.0, *)
  var dataDetectorTypes: UIDataDetectorTypes
  @available(iOS 6.0, *)
  var allowsEditingTextAttributes: Bool
  @available(iOS 6.0, *)
  @NSCopying var attributedText: NSAttributedString!
  @available(iOS 6.0, *)
  var typingAttributes: [String : AnyObject]
  func scrollRangeToVisible(_ range: NSRange)
  @available(iOS 6.0, *)
  var clearsOnInsertion: Bool
  @available(iOS 7.0, *)
  init(frame frame: CGRect, textContainer textContainer: NSTextContainer?)
  @available(iOS 7.0, *)
  var textContainer: NSTextContainer { get }
  @available(iOS 7.0, *)
  var textContainerInset: UIEdgeInsets
  @available(iOS 7.0, *)
  var layoutManager: NSLayoutManager { get }
  @available(iOS 7.0, *)
  var textStorage: NSTextStorage { get }
  @available(iOS 7.0, *)
  var linkTextAttributes: [String : AnyObject]!
}
let UITextViewTextDidBeginEditingNotification: String
let UITextViewTextDidChangeNotification: String
let UITextViewTextDidEndEditingNotification: String
