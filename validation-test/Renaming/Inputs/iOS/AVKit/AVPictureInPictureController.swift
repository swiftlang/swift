
@available(iOS 9.0, *)
class AVPictureInPictureController : NSObject {
  @discardableResult
  class func isPictureInPictureSupported() -> Bool
  @discardableResult
  class func pictureInPictureButtonStartImage(compatibleWith traitCollection: UITraitCollection?) -> UIImage
  @discardableResult
  class func pictureInPictureButtonStopImage(compatibleWith traitCollection: UITraitCollection?) -> UIImage
  weak var delegate: @sil_weak AVPictureInPictureControllerDelegate?
  func startPictureInPicture()
  func stopPictureInPicture()
  var isPictureInPicturePossible: Bool { get }
  var isPictureInPictureActive: Bool { get }
  var isPictureInPictureSuspended: Bool { get }
}
protocol AVPictureInPictureControllerDelegate : NSObjectProtocol {
  @available(iOS 9.0, *)
  optional func pictureInPictureControllerWillStartPicture(inPicture pictureInPictureController: AVPictureInPictureController)
  @available(iOS 9.0, *)
  optional func pictureInPictureControllerDidStartPicture(inPicture pictureInPictureController: AVPictureInPictureController)
  @available(iOS 9.0, *)
  optional func picture(_ pictureInPictureController: AVPictureInPictureController, failedToStartPictureInPictureWithError error: NSError)
  @available(iOS 9.0, *)
  optional func pictureInPictureControllerWillStopPicture(inPicture pictureInPictureController: AVPictureInPictureController)
  @available(iOS 9.0, *)
  optional func pictureInPictureControllerDidStopPicture(inPicture pictureInPictureController: AVPictureInPictureController)
  @available(iOS 9.0, *)
  optional func picture(_ pictureInPictureController: AVPictureInPictureController, restoreUserInterfaceForPictureInPictureStopWithCompletionHandler completionHandler: (Bool) -> Void)
}
