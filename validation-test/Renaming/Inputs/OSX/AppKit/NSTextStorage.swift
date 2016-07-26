
@available(OSX 10.11, *)
struct NSTextStorageEditActions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var editedAttributes: NSTextStorageEditActions { get }
  static var editedCharacters: NSTextStorageEditActions { get }
}
@available(OSX 10.0, *)
class NSTextStorage : NSMutableAttributedString {
  var layoutManagers: [NSLayoutManager] { get }
  func addLayoutManager(_ aLayoutManager: NSLayoutManager)
  func removeLayoutManager(_ aLayoutManager: NSLayoutManager)
  var editedMask: NSTextStorageEditActions { get }
  var editedRange: NSRange { get }
  var changeInLength: Int { get }
  unowned(unsafe) var delegate: @sil_unmanaged NSTextStorageDelegate?
  func edited(_ editedMask: NSTextStorageEditActions, range editedRange: NSRange, changeInLength delta: Int)
  func processEditing()
  var fixesAttributesLazily: Bool { get }
  func invalidateAttributes(in range: NSRange)
  func ensureAttributesAreFixed(in range: NSRange)
}
protocol NSTextStorageDelegate : NSObjectProtocol {
  @available(OSX 10.11, *)
  optional func textStorage(_ textStorage: NSTextStorage, willProcessEditing editedMask: NSTextStorageEditActions, range editedRange: NSRange, changeInLength delta: Int)
  @available(OSX 10.11, *)
  optional func textStorage(_ textStorage: NSTextStorage, didProcessEditing editedMask: NSTextStorageEditActions, range editedRange: NSRange, changeInLength delta: Int)
}
@available(OSX 10.0, *)
let NSTextStorageWillProcessEditingNotification: String
@available(OSX 10.0, *)
let NSTextStorageDidProcessEditingNotification: String
typealias NSTextStorageEditedOptions = Int
extension NSObject {
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -textStorage:willProcessEditing:range:changeInLength: instead.")
  class func textStorageWillProcessEditing(_ notification: NSNotification)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -textStorage:willProcessEditing:range:changeInLength: instead.")
  func textStorageWillProcessEditing(_ notification: NSNotification)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -textStorage:DidProcessEditing:range:changeInLength: instead.")
  class func textStorageDidProcessEditing(_ notification: NSNotification)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -textStorage:DidProcessEditing:range:changeInLength: instead.")
  func textStorageDidProcessEditing(_ notification: NSNotification)
}
