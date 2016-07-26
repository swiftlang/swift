
class NSUserDefaultsController : NSController {
  @discardableResult
  class func shared() -> NSUserDefaultsController
  init(defaults defaults: NSUserDefaults?, initialValues initialValues: [String : AnyObject]?)
  var defaults: NSUserDefaults { get }
  var initialValues: [String : AnyObject]?
  var appliesImmediately: Bool
  var hasUnappliedChanges: Bool { get }
  var values: AnyObject { get }
  func revert(_ sender: AnyObject?)
  func save(_ sender: AnyObject?)
  func revertToInitialValues(_ sender: AnyObject?)
}
struct __userDefaultsControllerFlags {
  var _sharedInstance: UInt32
  var _appliesImmediately: UInt32
  var _reservedUserDefaultsController: UInt32
  init()
  init(_sharedInstance _sharedInstance: UInt32, _appliesImmediately _appliesImmediately: UInt32, _reservedUserDefaultsController _reservedUserDefaultsController: UInt32)
}
