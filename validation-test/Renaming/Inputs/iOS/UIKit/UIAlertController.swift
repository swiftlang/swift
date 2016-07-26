
@available(iOS 8.0, *)
enum UIAlertActionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case cancel
  case destructive
}
@available(iOS 8.0, *)
enum UIAlertControllerStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case actionSheet
  case alert
}
@available(iOS 8.0, *)
class UIAlertAction : NSObject, NSCopying {
  convenience init(title title: String?, style style: UIAlertActionStyle, handler handler: ((UIAlertAction) -> Void)? = nil)
  var title: String? { get }
  var style: UIAlertActionStyle { get }
  var isEnabled: Bool
}
@available(iOS 8.0, *)
class UIAlertController : UIViewController {
  convenience init(title title: String?, message message: String?, preferredStyle preferredStyle: UIAlertControllerStyle)
  func addAction(_ action: UIAlertAction)
  var actions: [UIAlertAction] { get }
  @available(iOS 9.0, *)
  var preferredAction: UIAlertAction?
  func addTextField(configurationHandler configurationHandler: ((UITextField) -> Void)? = nil)
  var textFields: [UITextField]? { get }
  var message: String?
  var preferredStyle: UIAlertControllerStyle { get }
}
