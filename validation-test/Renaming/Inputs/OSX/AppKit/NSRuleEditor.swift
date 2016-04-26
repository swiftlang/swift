
enum NSRuleEditorNestingMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case single
  case list
  case compound
  case simple
}
enum NSRuleEditorRowType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case simple
  case compound
}
class NSRuleEditor : NSControl {
  unowned(unsafe) var delegate: @sil_unmanaged NSRuleEditorDelegate?
  var formattingStringsFilename: String?
  var formattingDictionary: [String : String]?
  func reloadCriteria()
  var nestingMode: NSRuleEditorNestingMode
  var rowHeight: CGFloat
  var isEditable: Bool
  var canRemoveAllRows: Bool
  var predicate: NSPredicate? { get }
  func reloadPredicate()
  @discardableResult
  func predicate(forRow row: Int) -> NSPredicate?
  var numberOfRows: Int { get }
  @discardableResult
  func subrowIndexes(forRow rowIndex: Int) -> NSIndexSet
  @discardableResult
  func criteria(forRow row: Int) -> [AnyObject]
  @discardableResult
  func displayValues(forRow row: Int) -> [AnyObject]
  @discardableResult
  func row(forDisplayValue displayValue: AnyObject) -> Int
  @discardableResult
  func rowType(forRow rowIndex: Int) -> NSRuleEditorRowType
  @discardableResult
  func parentRow(forRow rowIndex: Int) -> Int
  func addRow(_ sender: AnyObject?)
  func insertRow(at rowIndex: Int, with rowType: NSRuleEditorRowType, asSubrowOfRow parentRow: Int, animate shouldAnimate: Bool)
  func setCriteria(_ criteria: [AnyObject], andDisplayValues values: [AnyObject], forRowAt rowIndex: Int)
  func removeRow(at rowIndex: Int)
  func removeRows(at rowIndexes: NSIndexSet, includeSubrows includeSubrows: Bool)
  @NSCopying var selectedRowIndexes: NSIndexSet { get }
  func selectRowIndexes(_ indexes: NSIndexSet, byExtendingSelection extend: Bool)
  var rowClass: AnyClass
  var rowTypeKeyPath: String
  var subrowsKeyPath: String
  var criteriaKeyPath: String
  var displayValuesKeyPath: String
}
protocol NSRuleEditorDelegate : NSObjectProtocol {
  @discardableResult
  func ruleEditor(_ editor: NSRuleEditor, numberOfChildrenForCriterion criterion: AnyObject?, with rowType: NSRuleEditorRowType) -> Int
  @discardableResult
  func ruleEditor(_ editor: NSRuleEditor, child index: Int, forCriterion criterion: AnyObject?, with rowType: NSRuleEditorRowType) -> AnyObject
  @discardableResult
  func ruleEditor(_ editor: NSRuleEditor, displayValueForCriterion criterion: AnyObject, inRow row: Int) -> AnyObject
  @discardableResult
  optional func ruleEditor(_ editor: NSRuleEditor, predicatePartsForCriterion criterion: AnyObject, withDisplayValue value: AnyObject, inRow row: Int) -> [String : AnyObject]?
  optional func ruleEditorRowsDidChange(_ notification: NSNotification)
}
let NSRuleEditorPredicateLeftExpression: String
let NSRuleEditorPredicateRightExpression: String
let NSRuleEditorPredicateComparisonModifier: String
let NSRuleEditorPredicateOptions: String
let NSRuleEditorPredicateOperatorType: String
let NSRuleEditorPredicateCustomSelector: String
let NSRuleEditorPredicateCompoundType: String
let NSRuleEditorRowsDidChangeNotification: String
