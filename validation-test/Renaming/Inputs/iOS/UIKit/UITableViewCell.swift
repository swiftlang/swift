
enum UITableViewCellStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case value1
  case value2
  case subtitle
}
enum UITableViewCellSeparatorStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case singleLine
  case singleLineEtched
}
enum UITableViewCellSelectionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case blue
  case gray
  @available(iOS 7.0, *)
  case `default`
}
@available(iOS 9.0, *)
enum UITableViewCellFocusStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case custom
}
enum UITableViewCellEditingStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case delete
  case insert
}
enum UITableViewCellAccessoryType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case disclosureIndicator
  case detailDisclosureButton
  case checkmark
  @available(iOS 7.0, *)
  case detailButton
}
struct UITableViewCellStateMask : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var showingEditControlMask: UITableViewCellStateMask { get }
  static var showingDeleteConfirmationMask: UITableViewCellStateMask { get }
}
@available(iOS 2.0, *)
class UITableViewCell : UIView, NSCoding, UIGestureRecognizerDelegate {
  @available(iOS 3.0, *)
  init(style style: UITableViewCellStyle, reuseIdentifier reuseIdentifier: String?)
  @available(iOS 3.0, *)
  var imageView: UIImageView? { get }
  @available(iOS 3.0, *)
  var textLabel: UILabel? { get }
  @available(iOS 3.0, *)
  var detailTextLabel: UILabel? { get }
  var contentView: UIView { get }
  var backgroundView: UIView?
  var selectedBackgroundView: UIView?
  @available(iOS 5.0, *)
  var multipleSelectionBackgroundView: UIView?
  var reuseIdentifier: String? { get }
  func prepareForReuse()
  var selectionStyle: UITableViewCellSelectionStyle
  var isSelected: Bool
  var isHighlighted: Bool
  func setSelected(_ selected: Bool, animated animated: Bool)
  func setHighlighted(_ highlighted: Bool, animated animated: Bool)
  var editingStyle: UITableViewCellEditingStyle { get }
  var showsReorderControl: Bool
  var shouldIndentWhileEditing: Bool
  var accessoryType: UITableViewCellAccessoryType
  var accessoryView: UIView?
  var editingAccessoryType: UITableViewCellAccessoryType
  var editingAccessoryView: UIView?
  var indentationLevel: Int
  var indentationWidth: CGFloat
  @available(iOS 7.0, *)
  var separatorInset: UIEdgeInsets
  var isEditing: Bool
  func setEditing(_ editing: Bool, animated animated: Bool)
  var showingDeleteConfirmation: Bool { get }
  @available(iOS 9.0, *)
  var focusStyle: UITableViewCellFocusStyle
  @available(iOS 3.0, *)
  func willTransition(to state: UITableViewCellStateMask)
  @available(iOS 3.0, *)
  func didTransition(to state: UITableViewCellStateMask)
}
extension UITableViewCell {
}
