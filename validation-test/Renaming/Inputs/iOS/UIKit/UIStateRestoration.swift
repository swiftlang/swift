
@available(iOS 6.0, *)
let UIStateRestorationViewControllerStoryboardKey: String
@available(iOS 6.0, *)
let UIApplicationStateRestorationBundleVersionKey: String
@available(iOS 6.0, *)
let UIApplicationStateRestorationUserInterfaceIdiomKey: String
@available(iOS 7.0, *)
let UIApplicationStateRestorationTimestampKey: String
@available(iOS 7.0, *)
let UIApplicationStateRestorationSystemVersionKey: String
protocol UIViewControllerRestoration {
  @available(iOS 2.0, *)
  @discardableResult
  static func viewController(withRestorationIdentifierPath identifierComponents: [AnyObject], coder coder: NSCoder) -> UIViewController?
}
protocol UIDataSourceModelAssociation {
  @available(iOS 2.0, *)
  @discardableResult
  func modelIdentifierForElement(at idx: NSIndexPath, in view: UIView) -> String?
  @available(iOS 2.0, *)
  @discardableResult
  func indexPathForElement(withModelIdentifier identifier: String, in view: UIView) -> NSIndexPath?
}
protocol UIStateRestoring : NSObjectProtocol {
  optional var restorationParent: UIStateRestoring? { get }
  optional var objectRestorationClass: AnyObject.Type? { get }
  optional func encodeRestorableState(with coder: NSCoder)
  optional func decodeRestorableState(with coder: NSCoder)
  optional func applicationFinishedRestoringState()
}
protocol UIObjectRestoration {
  @discardableResult
  static func object(withRestorationIdentifierPath identifierComponents: [String], coder coder: NSCoder) -> UIStateRestoring?
}
