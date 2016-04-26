
enum UITableViewCellStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case value1
  case value2
  case subtitle
}
enum UITableViewCellSelectionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case blue
  case gray
  @available(tvOS 7.0, *)
  case `default`
}
@available(tvOS 9.0, *)
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
  case checkmark
}
struct UITableViewCellStateMask : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var showingEditControlMask: UITableViewCellStateMask { get }
  static var showingDeleteConfirmationMask: UITableViewCellStateMask { get }
}
@available(tvOS 2.0, *)
class UITableViewCell : UIView, NSCoding, UIGestureRecognizerDelegate {
  @available(tvOS 3.0, *)
  init(style style: UITableViewCellStyle, reuseIdentifier reuseIdentifier: String?)
  @available(tvOS 3.0, *)
  var imageView: UIImageView? { get }
  @available(tvOS 3.0, *)
  var textLabel: UILabel? { get }
  @available(tvOS 3.0, *)
  var detailTextLabel: UILabel? { get }
  var contentView: UIView { get }
  var backgroundView: UIView?
  var selectedBackgroundView: UIView?
  @available(tvOS 5.0, *)
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
  var isEditing: Bool
  func setEditing(_ editing: Bool, animated animated: Bool)
  var showingDeleteConfirmation: Bool { get }
  @available(tvOS 9.0, *)
  var focusStyle: UITableViewCellFocusStyle
  @available(tvOS 3.0, *)
  func willTransition(to state: UITableViewCellStateMask)
  @available(tvOS 3.0, *)
  func didTransition(to state: UITableViewCellStateMask)
}
extension UITableViewCell {
}
