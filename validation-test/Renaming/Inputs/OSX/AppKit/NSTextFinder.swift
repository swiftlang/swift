
@available(OSX 10.7, *)
enum NSTextFinderAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case showFindInterface
  case nextMatch
  case previousMatch
  case replaceAll
  case replace
  case replaceAndFind
  case setSearchString
  case replaceAllInSelection
  case selectAll
  case selectAllInSelection
  case hideFindInterface
  case showReplaceInterface
  case hideReplaceInterface
}
@available(OSX 10.7, *)
let NSTextFinderCaseInsensitiveKey: String
@available(OSX 10.7, *)
let NSTextFinderMatchingTypeKey: String
@available(OSX 10.7, *)
enum NSTextFinderMatchingType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case contains
  case startsWith
  case fullWord
  case endsWith
}
@available(OSX 10.7, *)
class NSTextFinder : NSObject, NSCoding {
  @IBOutlet unowned(unsafe) var client: @sil_unmanaged NSTextFinderClient?
  func perform(_ op: NSTextFinderAction)
  @discardableResult
  func validate(_ op: NSTextFinderAction) -> Bool
  @IBOutlet unowned(unsafe) var findBarContainer: @sil_unmanaged NSTextFinderBarContainer?
  func cancelFindIndicator()
  var findIndicatorNeedsUpdate: Bool
  var isIncrementalSearchingEnabled: Bool
  var incrementalSearchingShouldDimContentView: Bool
  var incrementalMatchRanges: [NSValue] { get }
  class func drawIncrementalMatchHighlight(in rect: NSRect)
  func noteClientStringWillChange()
}
protocol NSTextFinderClient : NSObjectProtocol {
  optional var isSelectable: Bool { get }
  optional var allowsMultipleSelection: Bool { get }
  optional var isEditable: Bool { get }
  optional var string: String { get }
  @discardableResult
  optional func string(at characterIndex: Int, effectiveRange outRange: NSRangePointer, endsWithSearchBoundary outFlag: UnsafeMutablePointer<ObjCBool>) -> String
  @discardableResult
  optional func stringLength() -> Int
  optional var firstSelectedRange: NSRange { get }
  optional var selectedRanges: [NSValue] { get set }
  optional func scrollRangeToVisible(_ range: NSRange)
  @discardableResult
  optional func shouldReplaceCharacters(inRanges ranges: [NSValue], with strings: [String]) -> Bool
  optional func replaceCharacters(in range: NSRange, with string: String)
  optional func didReplaceCharacters()
  @discardableResult
  optional func contentView(at index: Int, effectiveCharacterRange outRange: NSRangePointer) -> NSView
  @discardableResult
  optional func rects(forCharacterRange range: NSRange) -> [NSValue]?
  optional var visibleCharacterRanges: [NSValue] { get }
  optional func drawCharacters(in range: NSRange, forContentView view: NSView)
}
protocol NSTextFinderBarContainer : NSObjectProtocol {
  var findBarView: NSView? { get set }
  var isFindBarVisible: Bool { get set }
  func findBarViewDidChangeHeight()
  @discardableResult
  optional func contentView() -> NSView?
}
