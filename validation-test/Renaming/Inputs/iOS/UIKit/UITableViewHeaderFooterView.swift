
@available(iOS 6.0, *)
class UITableViewHeaderFooterView : UIView {
  init(reuseIdentifier reuseIdentifier: String?)
  var textLabel: UILabel? { get }
  var detailTextLabel: UILabel? { get }
  var contentView: UIView { get }
  var backgroundView: UIView?
  var reuseIdentifier: String? { get }
  func prepareForReuse()
}
