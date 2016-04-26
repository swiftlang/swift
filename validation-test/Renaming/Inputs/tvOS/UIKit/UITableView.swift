
enum UITableViewStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case plain
  case grouped
}
enum UITableViewScrollPosition : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case top
  case middle
  case bottom
}
enum UITableViewRowAnimation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case fade
  case right
  case left
  case top
  case bottom
  case none
  case middle
  case automatic
}
@available(tvOS 5.0, *)
let UITableViewAutomaticDimension: CGFloat
@available(tvOS 9.0, *)
class UITableViewFocusUpdateContext : UIFocusUpdateContext {
  var previouslyFocusedIndexPath: NSIndexPath? { get }
  var nextFocusedIndexPath: NSIndexPath? { get }
}
protocol UITableViewDelegate : NSObjectProtocol, UIScrollViewDelegate {
  @available(tvOS 2.0, *)
  optional func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: NSIndexPath)
  @available(tvOS 6.0, *)
  optional func tableView(_ tableView: UITableView, willDisplayHeaderView view: UIView, forSection section: Int)
  @available(tvOS 6.0, *)
  optional func tableView(_ tableView: UITableView, willDisplayFooterView view: UIView, forSection section: Int)
  @available(tvOS 6.0, *)
  optional func tableView(_ tableView: UITableView, didEndDisplaying cell: UITableViewCell, forRowAt indexPath: NSIndexPath)
  @available(tvOS 6.0, *)
  optional func tableView(_ tableView: UITableView, didEndDisplayingHeaderView view: UIView, forSection section: Int)
  @available(tvOS 6.0, *)
  optional func tableView(_ tableView: UITableView, didEndDisplayingFooterView view: UIView, forSection section: Int)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, heightForRowAt indexPath: NSIndexPath) -> CGFloat
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat
  @available(tvOS 7.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, estimatedHeightForRowAt indexPath: NSIndexPath) -> CGFloat
  @available(tvOS 7.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, estimatedHeightForHeaderInSection section: Int) -> CGFloat
  @available(tvOS 7.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, estimatedHeightForFooterInSection section: Int) -> CGFloat
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView?
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, viewForFooterInSection section: Int) -> UIView?
  @available(tvOS 2.0, *)
  optional func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: NSIndexPath)
  @available(tvOS 6.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, shouldHighlightRowAt indexPath: NSIndexPath) -> Bool
  @available(tvOS 6.0, *)
  optional func tableView(_ tableView: UITableView, didHighlightRowAt indexPath: NSIndexPath)
  @available(tvOS 6.0, *)
  optional func tableView(_ tableView: UITableView, didUnhighlightRowAt indexPath: NSIndexPath)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, willSelectRowAt indexPath: NSIndexPath) -> NSIndexPath?
  @available(tvOS 3.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, willDeselectRowAt indexPath: NSIndexPath) -> NSIndexPath?
  @available(tvOS 2.0, *)
  optional func tableView(_ tableView: UITableView, didSelectRowAt indexPath: NSIndexPath)
  @available(tvOS 3.0, *)
  optional func tableView(_ tableView: UITableView, didDeselectRowAt indexPath: NSIndexPath)
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, editingStyleForRowAt indexPath: NSIndexPath) -> UITableViewCellEditingStyle
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, shouldIndentWhileEditingRowAt indexPath: NSIndexPath) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, targetIndexPathForMoveFromRowAt sourceIndexPath: NSIndexPath, toProposedIndexPath proposedDestinationIndexPath: NSIndexPath) -> NSIndexPath
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, indentationLevelForRowAt indexPath: NSIndexPath) -> Int
  @available(tvOS 5.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, shouldShowMenuForRowAt indexPath: NSIndexPath) -> Bool
  @available(tvOS 5.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, canPerformAction action: Selector, forRowAt indexPath: NSIndexPath, withSender sender: AnyObject?) -> Bool
  @available(tvOS 5.0, *)
  optional func tableView(_ tableView: UITableView, performAction action: Selector, forRowAt indexPath: NSIndexPath, withSender sender: AnyObject?)
  @available(tvOS 9.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, canFocusRowAt indexPath: NSIndexPath) -> Bool
  @available(tvOS 9.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, shouldUpdateFocusIn context: UITableViewFocusUpdateContext) -> Bool
  @available(tvOS 9.0, *)
  optional func tableView(_ tableView: UITableView, didUpdateFocusIn context: UITableViewFocusUpdateContext, with coordinator: UIFocusAnimationCoordinator)
  @available(tvOS 9.0, *)
  @discardableResult
  optional func indexPathForPreferredFocusedView(in tableView: UITableView) -> NSIndexPath?
}
let UITableViewSelectionDidChangeNotification: String
@available(tvOS 2.0, *)
class UITableView : UIScrollView, NSCoding {
  init(frame frame: CGRect, style style: UITableViewStyle)
  var style: UITableViewStyle { get }
  weak var dataSource: @sil_weak UITableViewDataSource?
  var rowHeight: CGFloat
  var sectionHeaderHeight: CGFloat
  var sectionFooterHeight: CGFloat
  @available(tvOS 7.0, *)
  var estimatedRowHeight: CGFloat
  @available(tvOS 7.0, *)
  var estimatedSectionHeaderHeight: CGFloat
  @available(tvOS 7.0, *)
  var estimatedSectionFooterHeight: CGFloat
  @available(tvOS 7.0, *)
  var separatorInset: UIEdgeInsets
  @available(tvOS 3.2, *)
  var backgroundView: UIView?
  func reloadData()
  @available(tvOS 3.0, *)
  func reloadSectionIndexTitles()
  var numberOfSections: Int { get }
  @discardableResult
  func numberOfRows(inSection section: Int) -> Int
  @discardableResult
  func rect(forSection section: Int) -> CGRect
  @discardableResult
  func rectForHeader(inSection section: Int) -> CGRect
  @discardableResult
  func rectForFooter(inSection section: Int) -> CGRect
  @discardableResult
  func rectForRow(at indexPath: NSIndexPath) -> CGRect
  @discardableResult
  func indexPathForRow(at point: CGPoint) -> NSIndexPath?
  @discardableResult
  func indexPath(for cell: UITableViewCell) -> NSIndexPath?
  @discardableResult
  func indexPathsForRows(in rect: CGRect) -> [NSIndexPath]?
  @discardableResult
  func cellForRow(at indexPath: NSIndexPath) -> UITableViewCell?
  var visibleCells: [UITableViewCell] { get }
  var indexPathsForVisibleRows: [NSIndexPath]? { get }
  @available(tvOS 6.0, *)
  @discardableResult
  func headerView(forSection section: Int) -> UITableViewHeaderFooterView?
  @available(tvOS 6.0, *)
  @discardableResult
  func footerView(forSection section: Int) -> UITableViewHeaderFooterView?
  func scrollToRow(at indexPath: NSIndexPath, at scrollPosition: UITableViewScrollPosition, animated animated: Bool)
  func scrollToNearestSelectedRow(at scrollPosition: UITableViewScrollPosition, animated animated: Bool)
  func beginUpdates()
  func endUpdates()
  func insertSections(_ sections: NSIndexSet, with animation: UITableViewRowAnimation)
  func deleteSections(_ sections: NSIndexSet, with animation: UITableViewRowAnimation)
  @available(tvOS 3.0, *)
  func reloadSections(_ sections: NSIndexSet, with animation: UITableViewRowAnimation)
  @available(tvOS 5.0, *)
  func moveSection(_ section: Int, toSection newSection: Int)
  func insertRows(at indexPaths: [NSIndexPath], with animation: UITableViewRowAnimation)
  func deleteRows(at indexPaths: [NSIndexPath], with animation: UITableViewRowAnimation)
  @available(tvOS 3.0, *)
  func reloadRows(at indexPaths: [NSIndexPath], with animation: UITableViewRowAnimation)
  @available(tvOS 5.0, *)
  func moveRow(at indexPath: NSIndexPath, to newIndexPath: NSIndexPath)
  var isEditing: Bool
  func setEditing(_ editing: Bool, animated animated: Bool)
  @available(tvOS 3.0, *)
  var allowsSelection: Bool
  var allowsSelectionDuringEditing: Bool
  @available(tvOS 5.0, *)
  var allowsMultipleSelection: Bool
  @available(tvOS 5.0, *)
  var allowsMultipleSelectionDuringEditing: Bool
  var indexPathForSelectedRow: NSIndexPath? { get }
  @available(tvOS 5.0, *)
  var indexPathsForSelectedRows: [NSIndexPath]? { get }
  func selectRow(at indexPath: NSIndexPath?, animated animated: Bool, scrollPosition scrollPosition: UITableViewScrollPosition)
  func deselectRow(at indexPath: NSIndexPath, animated animated: Bool)
  var sectionIndexMinimumDisplayRowCount: Int
  @available(tvOS 6.0, *)
  var sectionIndexColor: UIColor?
  @available(tvOS 7.0, *)
  var sectionIndexBackgroundColor: UIColor?
  @available(tvOS 6.0, *)
  var sectionIndexTrackingBackgroundColor: UIColor?
  @available(tvOS 9.0, *)
  var cellLayoutMarginsFollowReadableWidth: Bool
  var tableHeaderView: UIView?
  var tableFooterView: UIView?
  @discardableResult
  func dequeueReusableCell(withIdentifier identifier: String) -> UITableViewCell?
  @available(tvOS 6.0, *)
  @discardableResult
  func dequeueReusableCell(withIdentifier identifier: String, for indexPath: NSIndexPath) -> UITableViewCell
  @available(tvOS 6.0, *)
  @discardableResult
  func dequeueReusableHeaderFooterView(withIdentifier identifier: String) -> UITableViewHeaderFooterView?
  @available(tvOS 5.0, *)
  func register(_ nib: UINib?, forCellReuseIdentifier identifier: String)
  @available(tvOS 6.0, *)
  func register(_ cellClass: AnyClass?, forCellReuseIdentifier identifier: String)
  @available(tvOS 6.0, *)
  func register(_ nib: UINib?, forHeaderFooterViewReuseIdentifier identifier: String)
  @available(tvOS 6.0, *)
  func register(_ aClass: AnyClass?, forHeaderFooterViewReuseIdentifier identifier: String)
  @available(tvOS 9.0, *)
  var remembersLastFocusedIndexPath: Bool
}
protocol UITableViewDataSource : NSObjectProtocol {
  @available(tvOS 2.0, *)
  @discardableResult
  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int
  @available(tvOS 2.0, *)
  @discardableResult
  func tableView(_ tableView: UITableView, cellForRowAt indexPath: NSIndexPath) -> UITableViewCell
  @available(tvOS 2.0, *)
  @discardableResult
  optional func numberOfSections(in tableView: UITableView) -> Int
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String?
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, titleForFooterInSection section: Int) -> String?
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, canEditRowAt indexPath: NSIndexPath) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  optional func tableView(_ tableView: UITableView, canMoveRowAt indexPath: NSIndexPath) -> Bool
  @available(tvOS 2.0, *)
  optional func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCellEditingStyle, forRowAt indexPath: NSIndexPath)
  @available(tvOS 2.0, *)
  optional func tableView(_ tableView: UITableView, moveRowAt sourceIndexPath: NSIndexPath, to destinationIndexPath: NSIndexPath)
}
extension NSIndexPath {
  convenience init(forRow row: Int, inSection section: Int)
  var section: Int { get }
  var row: Int { get }
}
