
protocol PKAddPassesViewControllerDelegate : NSObjectProtocol {
  @available(iOS 6.0, *)
  optional func addPassesViewControllerDidFinish(_ controller: PKAddPassesViewController)
}
@available(iOS 6.0, *)
class PKAddPassesViewController : UIViewController {
  init(pass pass: PKPass)
  @available(iOS 7.0, *)
  init(passes passes: [PKPass])
  @available(iOS 8.0, *)
  @discardableResult
  class func canAddPasses() -> Bool
  unowned(unsafe) var delegate: @sil_unmanaged PKAddPassesViewControllerDelegate?
}
