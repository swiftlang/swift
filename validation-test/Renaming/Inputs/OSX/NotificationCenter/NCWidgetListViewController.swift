
@available(OSX 10.10, *)
class NCWidgetListViewController : NSViewController {
  @IBOutlet weak var delegate: @sil_weak NCWidgetListViewDelegate!
  var contents: [AnyObject]!
  var minimumVisibleRowCount: Int
  var hasDividerLines: Bool
  var editing: Bool
  var showsAddButtonWhenEditing: Bool
  @discardableResult
  func viewController(atRow row: Int, makeIfNecessary makeIfNecesary: Bool) -> NSViewController!
  @discardableResult
  func row(for viewController: NSViewController!) -> Int
}
protocol NCWidgetListViewDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  @discardableResult
  func widgetList(_ list: NCWidgetListViewController!, viewControllerForRow row: Int) -> NSViewController!
  @available(OSX 10.10, *)
  optional func widgetListPerformAddAction(_ list: NCWidgetListViewController!)
  @available(OSX 10.10, *)
  @discardableResult
  optional func widgetList(_ list: NCWidgetListViewController!, shouldReorderRow row: Int) -> Bool
  @available(OSX 10.10, *)
  optional func widgetList(_ list: NCWidgetListViewController!, didReorderRow row: Int, toRow newIndex: Int)
  @available(OSX 10.10, *)
  @discardableResult
  optional func widgetList(_ list: NCWidgetListViewController!, shouldRemoveRow row: Int) -> Bool
  @available(OSX 10.10, *)
  optional func widgetList(_ list: NCWidgetListViewController!, didRemoveRow row: Int)
}
