
enum UIImagePickerControllerSourceType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case photoLibrary
  case camera
  case savedPhotosAlbum
}
enum UIImagePickerControllerQualityType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case typeHigh
  case typeMedium
  case typeLow
  @available(iOS 4.0, *)
  case type640x480
  @available(iOS 5.0, *)
  case typeIFrame1280x720
  @available(iOS 5.0, *)
  case typeIFrame960x540
}
enum UIImagePickerControllerCameraCaptureMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case photo
  case video
}
enum UIImagePickerControllerCameraDevice : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case rear
  case front
}
enum UIImagePickerControllerCameraFlashMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case off
  case auto
  case on
}
let UIImagePickerControllerMediaType: String
let UIImagePickerControllerOriginalImage: String
let UIImagePickerControllerEditedImage: String
let UIImagePickerControllerCropRect: String
let UIImagePickerControllerMediaURL: String
@available(iOS 4.1, *)
let UIImagePickerControllerReferenceURL: String
@available(iOS 4.1, *)
let UIImagePickerControllerMediaMetadata: String
@available(iOS 9.1, *)
let UIImagePickerControllerLivePhoto: String
@available(iOS 2.0, *)
class UIImagePickerController : UINavigationController, NSCoding {
  @discardableResult
  class func isSourceTypeAvailable(_ sourceType: UIImagePickerControllerSourceType) -> Bool
  @discardableResult
  class func availableMediaTypes(for sourceType: UIImagePickerControllerSourceType) -> [String]?
  @available(iOS 4.0, *)
  @discardableResult
  class func isCameraDeviceAvailable(_ cameraDevice: UIImagePickerControllerCameraDevice) -> Bool
  @available(iOS 4.0, *)
  @discardableResult
  class func isFlashAvailable(for cameraDevice: UIImagePickerControllerCameraDevice) -> Bool
  @available(iOS 4.0, *)
  @discardableResult
  class func availableCaptureModes(for cameraDevice: UIImagePickerControllerCameraDevice) -> [NSNumber]?
  var sourceType: UIImagePickerControllerSourceType
  var mediaTypes: [String]
  @available(iOS 3.1, *)
  var allowsEditing: Bool
  @available(iOS 3.1, *)
  var videoMaximumDuration: NSTimeInterval
  @available(iOS 3.1, *)
  var videoQuality: UIImagePickerControllerQualityType
  @available(iOS 3.1, *)
  var showsCameraControls: Bool
  @available(iOS 3.1, *)
  var cameraOverlayView: UIView?
  @available(iOS 3.1, *)
  var cameraViewTransform: CGAffineTransform
  @available(iOS 3.1, *)
  func takePicture()
  @available(iOS 4.0, *)
  @discardableResult
  func startVideoCapture() -> Bool
  @available(iOS 4.0, *)
  func stopVideoCapture()
  @available(iOS 4.0, *)
  var cameraCaptureMode: UIImagePickerControllerCameraCaptureMode
  @available(iOS 4.0, *)
  var cameraDevice: UIImagePickerControllerCameraDevice
  @available(iOS 4.0, *)
  var cameraFlashMode: UIImagePickerControllerCameraFlashMode
}
protocol UIImagePickerControllerDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  optional func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : AnyObject])
  @available(iOS 2.0, *)
  optional func imagePickerControllerDidCancel(_ picker: UIImagePickerController)
}
func UIImageWriteToSavedPhotosAlbum(_ image: UIImage, _ completionTarget: AnyObject?, _ completionSelector: Selector?, _ contextInfo: UnsafeMutablePointer<Void>?)
@available(iOS 3.1, *)
@discardableResult
func UIVideoAtPathIsCompatibleWithSavedPhotosAlbum(_ videoPath: String) -> Bool
@available(iOS 3.1, *)
func UISaveVideoAtPathToSavedPhotosAlbum(_ videoPath: String, _ completionTarget: AnyObject?, _ completionSelector: Selector?, _ contextInfo: UnsafeMutablePointer<Void>?)
