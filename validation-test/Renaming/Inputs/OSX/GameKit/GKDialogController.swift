
protocol GKViewController {
}
class GKDialogController : NSResponder {
  @IBOutlet unowned(unsafe) var parentWindow: @sil_unmanaged NSWindow?
  @discardableResult
  func present(_ viewController: NSViewController) -> Bool
  @IBAction func dismiss(_ sender: AnyObject)
}
extension GKDialogController {
  @discardableResult
  class func shared() -> GKDialogController
}
