
let NSComboBoxWillPopUpNotification: String
let NSComboBoxWillDismissNotification: String
let NSComboBoxSelectionDidChangeNotification: String
let NSComboBoxSelectionIsChangingNotification: String
class NSComboBox : NSTextField {
  var hasVerticalScroller: Bool
  var intercellSpacing: NSSize
  var itemHeight: CGFloat
  var numberOfVisibleItems: Int
  var isButtonBordered: Bool
  func reloadData()
  func noteNumberOfItemsChanged()
  var usesDataSource: Bool
  func scrollItemAtIndex(toTop index: Int)
  func scrollItemAtIndexToVisible(_ index: Int)
  func selectItem(at index: Int)
  func deselectItem(at index: Int)
  var indexOfSelectedItem: Int { get }
  var numberOfItems: Int { get }
  var completes: Bool
  unowned(unsafe) var dataSource: @sil_unmanaged NSComboBoxDataSource?
  func addItem(with object: AnyObject)
  func addItems(with objects: [AnyObject])
  func insertItem(with object: AnyObject, at index: Int)
  func removeItem(with object: AnyObject)
  func removeItem(at index: Int)
  func removeAllItems()
  func selectItem(with object: AnyObject?)
  @discardableResult
  func itemObjectValue(at index: Int) -> AnyObject
  var objectValueOfSelectedItem: AnyObject? { get }
  @discardableResult
  func indexOfItem(with object: AnyObject) -> Int
  var objectValues: [AnyObject] { get }
}
protocol NSComboBoxDataSource : NSObjectProtocol {
  @discardableResult
  optional func numberOfItems(in aComboBox: NSComboBox) -> Int
  @discardableResult
  optional func comboBox(_ aComboBox: NSComboBox, objectValueForItemAt index: Int) -> AnyObject
  @discardableResult
  optional func comboBox(_ aComboBox: NSComboBox, indexOfItemWithStringValue string: String) -> Int
  @discardableResult
  optional func comboBox(_ aComboBox: NSComboBox, completedString string: String) -> String?
}
protocol NSComboBoxDelegate : NSTextFieldDelegate {
  optional func comboBoxWillPopUp(_ notification: NSNotification)
  optional func comboBoxWillDismiss(_ notification: NSNotification)
  optional func comboBoxSelectionDidChange(_ notification: NSNotification)
  optional func comboBoxSelectionIsChanging(_ notification: NSNotification)
}
