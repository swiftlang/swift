
@available(tvOS 5.0, *)
class GLKViewController : UIViewController, NSCoding, GLKViewDelegate {
  @IBOutlet unowned(unsafe) var delegate: @sil_unmanaged GLKViewControllerDelegate?
  var preferredFramesPerSecond: Int
  var framesPerSecond: Int { get }
  var isPaused: Bool
  var framesDisplayed: Int { get }
  var timeSinceFirstResume: NSTimeInterval { get }
  var timeSinceLastResume: NSTimeInterval { get }
  var timeSinceLastUpdate: NSTimeInterval { get }
  var timeSinceLastDraw: NSTimeInterval { get }
  var pauseOnWillResignActive: Bool
  var resumeOnDidBecomeActive: Bool
}
protocol GLKViewControllerDelegate : NSObjectProtocol {
  @available(tvOS 5.0, *)
  func glkViewControllerUpdate(_ controller: GLKViewController)
  @available(tvOS 5.0, *)
  optional func glkViewController(_ controller: GLKViewController, willPause pause: Bool)
}
