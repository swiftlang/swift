
protocol UIKeyInput : UITextInputTraits {
  @discardableResult
  func hasText() -> Bool
  func insertText(_ text: String)
  func deleteBackward()
}
enum UITextStorageDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case forward
  case backward
}
enum UITextLayoutDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case right
  case left
  case up
  case down
}
typealias UITextDirection = Int
enum UITextWritingDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case natural
  case leftToRight
  case rightToLeft
}
enum UITextGranularity : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case character
  case word
  case sentence
  case paragraph
  case line
  case document
}
@available(iOS 5.1, *)
class UIDictationPhrase : NSObject {
  var text: String { get }
  var alternativeInterpretations: [String]? { get }
}
@available(iOS 9.0, *)
class UITextInputAssistantItem : NSObject {
  var allowsHidingShortcuts: Bool
  var leadingBarButtonGroups: [UIBarButtonItemGroup]
  var trailingBarButtonGroups: [UIBarButtonItemGroup]
}
protocol UITextInput : UIKeyInput {
  @available(iOS 3.2, *)
  @discardableResult
  func text(in range: UITextRange) -> String?
  @available(iOS 3.2, *)
  func replace(_ range: UITextRange, withText text: String)
  @available(iOS 3.2, *)
  @NSCopying var selectedTextRange: UITextRange? { get set }
  @available(iOS 3.2, *)
  var markedTextRange: UITextRange? { get }
  @available(iOS 2.0, *)
  var markedTextStyle: [NSObject : AnyObject]? { get set }
  func setMarkedText(_ markedText: String?, selectedRange selectedRange: NSRange)
  func unmarkText()
  @available(iOS 3.2, *)
  var beginningOfDocument: UITextPosition { get }
  @available(iOS 3.2, *)
  var endOfDocument: UITextPosition { get }
  @available(iOS 3.2, *)
  @discardableResult
  func textRange(from fromPosition: UITextPosition, to toPosition: UITextPosition) -> UITextRange?
  @available(iOS 3.2, *)
  @discardableResult
  func position(from position: UITextPosition, offset offset: Int) -> UITextPosition?
  @available(iOS 3.2, *)
  @discardableResult
  func position(from position: UITextPosition, in direction: UITextLayoutDirection, offset offset: Int) -> UITextPosition?
  @available(iOS 3.2, *)
  @discardableResult
  func compare(_ position: UITextPosition, to other: UITextPosition) -> NSComparisonResult
  @available(iOS 3.2, *)
  @discardableResult
  func offset(from from: UITextPosition, to toPosition: UITextPosition) -> Int
  weak var inputDelegate: @sil_weak UITextInputDelegate? { get set }
  var tokenizer: UITextInputTokenizer { get }
  @available(iOS 3.2, *)
  @discardableResult
  func position(within range: UITextRange, farthestIn direction: UITextLayoutDirection) -> UITextPosition?
  @available(iOS 3.2, *)
  @discardableResult
  func characterRange(byExtending position: UITextPosition, in direction: UITextLayoutDirection) -> UITextRange?
  @available(iOS 3.2, *)
  @discardableResult
  func baseWritingDirection(for position: UITextPosition, in direction: UITextStorageDirection) -> UITextWritingDirection
  @available(iOS 3.2, *)
  func setBaseWritingDirection(_ writingDirection: UITextWritingDirection, for range: UITextRange)
  @available(iOS 3.2, *)
  @discardableResult
  func firstRect(for range: UITextRange) -> CGRect
  @available(iOS 3.2, *)
  @discardableResult
  func caretRect(for position: UITextPosition) -> CGRect
  @available(iOS 6.0, *)
  @discardableResult
  func selectionRects(for range: UITextRange) -> [AnyObject]
  @available(iOS 3.2, *)
  @discardableResult
  func closestPosition(to point: CGPoint) -> UITextPosition?
  @available(iOS 3.2, *)
  @discardableResult
  func closestPosition(to point: CGPoint, within range: UITextRange) -> UITextPosition?
  @available(iOS 3.2, *)
  @discardableResult
  func characterRange(at point: CGPoint) -> UITextRange?
  @available(iOS 6.0, *)
  @discardableResult
  optional func shouldChangeText(in range: UITextRange, replacementText text: String) -> Bool
  @available(iOS 3.2, *)
  @discardableResult
  optional func textStyling(at position: UITextPosition, in direction: UITextStorageDirection) -> [String : AnyObject]?
  @available(iOS 3.2, *)
  @discardableResult
  optional func position(within range: UITextRange, atCharacterOffset offset: Int) -> UITextPosition?
  @available(iOS 3.2, *)
  @discardableResult
  optional func characterOffset(of position: UITextPosition, within range: UITextRange) -> Int
  @available(iOS 2.0, *)
  optional var textInputView: UIView { get }
  optional var selectionAffinity: UITextStorageDirection { get set }
  @available(iOS 5.1, *)
  optional func insertDictationResult(_ dictationResult: [UIDictationPhrase])
  optional func dictationRecordingDidEnd()
  optional func dictationRecognitionFailed()
  @discardableResult
  optional func insertDictationResultPlaceholder() -> AnyObject
  @discardableResult
  optional func frame(forDictationResultPlaceholder placeholder: AnyObject) -> CGRect
  optional func removeDictationResultPlaceholder(_ placeholder: AnyObject, willInsertResult willInsertResult: Bool)
  @available(iOS 9.0, *)
  optional func beginFloatingCursor(at point: CGPoint)
  @available(iOS 9.0, *)
  optional func updateFloatingCursor(at point: CGPoint)
  @available(iOS 9.0, *)
  optional func endFloatingCursor()
}
@available(iOS, introduced: 3.2, deprecated: 8.0, message: "Use NSBackgroundColorAttributeName instead")
let UITextInputTextBackgroundColorKey: String
@available(iOS, introduced: 3.2, deprecated: 8.0, message: "Use NSForegroundColorAttributeName instead")
let UITextInputTextColorKey: String
@available(iOS, introduced: 3.2, deprecated: 8.0, message: "Use NSFontAttributeName instead")
let UITextInputTextFontKey: String
@available(iOS 3.2, *)
class UITextPosition : NSObject {
}
@available(iOS 3.2, *)
class UITextRange : NSObject {
  var isEmpty: Bool { get }
  var start: UITextPosition { get }
  var end: UITextPosition { get }
}
@available(iOS 6.0, *)
class UITextSelectionRect : NSObject {
  var rect: CGRect { get }
  var writingDirection: UITextWritingDirection { get }
  var containsStart: Bool { get }
  var containsEnd: Bool { get }
  var isVertical: Bool { get }
}
protocol UITextInputDelegate : NSObjectProtocol {
  func selectionWillChange(_ textInput: UITextInput?)
  func selectionDidChange(_ textInput: UITextInput?)
  func textWillChange(_ textInput: UITextInput?)
  func textDidChange(_ textInput: UITextInput?)
}
protocol UITextInputTokenizer : NSObjectProtocol {
  @available(iOS 3.2, *)
  @discardableResult
  func rangeEnclosingPosition(_ position: UITextPosition, with granularity: UITextGranularity, inDirection direction: UITextDirection) -> UITextRange?
  @available(iOS 3.2, *)
  @discardableResult
  func isPosition(_ position: UITextPosition, atBoundary granularity: UITextGranularity, inDirection direction: UITextDirection) -> Bool
  @available(iOS 3.2, *)
  @discardableResult
  func position(from position: UITextPosition, toBoundary granularity: UITextGranularity, inDirection direction: UITextDirection) -> UITextPosition?
  @available(iOS 3.2, *)
  @discardableResult
  func isPosition(_ position: UITextPosition, withinTextUnit granularity: UITextGranularity, inDirection direction: UITextDirection) -> Bool
}
@available(iOS 3.2, *)
class UITextInputStringTokenizer : NSObject, UITextInputTokenizer {
  init(textInput textInput: UIResponder)
}
@available(iOS 4.2, *)
class UITextInputMode : NSObject, NSSecureCoding {
  var primaryLanguage: String? { get }
  @discardableResult
  class func activeInputModes() -> [String]
}
@available(iOS 4.2, *)
let UITextInputCurrentInputModeDidChangeNotification: String
