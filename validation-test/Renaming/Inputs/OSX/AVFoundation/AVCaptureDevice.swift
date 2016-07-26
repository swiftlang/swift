
@available(OSX 10.7, *)
let AVCaptureDeviceWasConnectedNotification: String
@available(OSX 10.7, *)
let AVCaptureDeviceWasDisconnectedNotification: String
@available(OSX 10.7, *)
class AVCaptureDevice : NSObject {
  @discardableResult
  class func devices() -> [AnyObject]!
  @discardableResult
  class func devices(withMediaType mediaType: String!) -> [AnyObject]!
  @discardableResult
  class func defaultDevice(withMediaType mediaType: String!) -> AVCaptureDevice!
  /*not inherited*/ init!(uniqueID deviceUniqueID: String!)
  var uniqueID: String! { get }
  var modelID: String! { get }
  var localizedName: String! { get }
  @available(OSX 10.9, *)
  var manufacturer: String! { get }
  @available(OSX 10.7, *)
  var transportType: Int32 { get }
  @discardableResult
  func hasMediaType(_ mediaType: String!) -> Bool
  func lockForConfiguration() throws
  func unlockForConfiguration()
  @discardableResult
  func supportsAVCaptureSessionPreset(_ preset: String!) -> Bool
  var isConnected: Bool { get }
  @available(OSX 10.7, *)
  var isInUseByAnotherApplication: Bool { get }
  @available(OSX 10.7, *)
  var isSuspended: Bool { get }
  @available(OSX 10.7, *)
  var linkedDevices: [AnyObject]! { get }
  @available(OSX 10.7, *)
  var formats: [AnyObject]! { get }
  @available(OSX 10.7, *)
  var activeFormat: AVCaptureDeviceFormat!
  @available(OSX 10.7, *)
  var activeVideoMinFrameDuration: CMTime
  @available(OSX 10.9, *)
  var activeVideoMaxFrameDuration: CMTime
  @available(OSX 10.7, *)
  var inputSources: [AnyObject]! { get }
  @available(OSX 10.7, *)
  var activeInputSource: AVCaptureDeviceInputSource!
}
@available(OSX 10.7, *)
enum AVCaptureDevicePosition : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unspecified
  case back
  case front
}
extension AVCaptureDevice {
  var position: AVCaptureDevicePosition { get }
}
@available(OSX 10.7, *)
enum AVCaptureFlashMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case off
  case on
  case auto
}
extension AVCaptureDevice {
  var hasFlash: Bool { get }
  @discardableResult
  func isFlashModeSupported(_ flashMode: AVCaptureFlashMode) -> Bool
  var flashMode: AVCaptureFlashMode
}
@available(OSX 10.7, *)
enum AVCaptureTorchMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case off
  case on
  case auto
}
let AVCaptureMaxAvailableTorchLevel: Float
extension AVCaptureDevice {
  var hasTorch: Bool { get }
  @discardableResult
  func isTorchModeSupported(_ torchMode: AVCaptureTorchMode) -> Bool
  var torchMode: AVCaptureTorchMode
}
@available(OSX 10.7, *)
enum AVCaptureFocusMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case locked
  case autoFocus
  case continuousAutoFocus
}
extension AVCaptureDevice {
  @discardableResult
  func isFocusModeSupported(_ focusMode: AVCaptureFocusMode) -> Bool
  var focusMode: AVCaptureFocusMode
  var isFocusPointOfInterestSupported: Bool { get }
  var focusPointOfInterest: CGPoint
  var isAdjustingFocus: Bool { get }
}
@available(OSX 10.7, *)
enum AVCaptureExposureMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case locked
  case autoExpose
  case continuousAutoExposure
}
extension AVCaptureDevice {
  @discardableResult
  func isExposureModeSupported(_ exposureMode: AVCaptureExposureMode) -> Bool
  var exposureMode: AVCaptureExposureMode
  var isExposurePointOfInterestSupported: Bool { get }
  var exposurePointOfInterest: CGPoint
  var isAdjustingExposure: Bool { get }
}
@available(OSX 10.7, *)
enum AVCaptureWhiteBalanceMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case locked
  case autoWhiteBalance
  case continuousAutoWhiteBalance
}
extension AVCaptureDevice {
  @discardableResult
  func isWhiteBalanceModeSupported(_ whiteBalanceMode: AVCaptureWhiteBalanceMode) -> Bool
  var whiteBalanceMode: AVCaptureWhiteBalanceMode
  var isAdjustingWhiteBalance: Bool { get }
}
extension AVCaptureDevice {
}
extension AVCaptureDevice {
}
extension AVCaptureDevice {
}
extension AVCaptureDevice {
}
typealias AVCaptureDeviceTransportControlsSpeed = Float
@available(OSX 10.7, *)
enum AVCaptureDeviceTransportControlsPlaybackMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notPlayingMode
  case playingMode
}
extension AVCaptureDevice {
  @available(OSX 10.7, *)
  var transportControlsSupported: Bool { get }
  @available(OSX 10.7, *)
  var transportControlsPlaybackMode: AVCaptureDeviceTransportControlsPlaybackMode { get }
  @available(OSX 10.7, *)
  var transportControlsSpeed: AVCaptureDeviceTransportControlsSpeed { get }
  @available(OSX 10.7, *)
  func setTransportControlsPlaybackMode(_ mode: AVCaptureDeviceTransportControlsPlaybackMode, speed speed: AVCaptureDeviceTransportControlsSpeed)
}
extension AVCaptureDevice {
}
@available(OSX 10.7, *)
class AVFrameRateRange : NSObject {
  var minFrameRate: Float64 { get }
  var maxFrameRate: Float64 { get }
  var maxFrameDuration: CMTime { get }
  var minFrameDuration: CMTime { get }
}
@available(OSX 10.7, *)
class AVCaptureDeviceFormat : NSObject {
  var mediaType: String! { get }
  var formatDescription: CMFormatDescription! { get }
  var videoSupportedFrameRateRanges: [AnyObject]! { get }
}
@available(OSX 10.7, *)
class AVCaptureDeviceInputSource : NSObject {
  var inputSourceID: String! { get }
  var localizedName: String! { get }
}
