
@available(iOS 4.0, *)
let AVCaptureDeviceWasConnectedNotification: String
@available(iOS 4.0, *)
let AVCaptureDeviceWasDisconnectedNotification: String
@available(iOS 5.0, *)
let AVCaptureDeviceSubjectAreaDidChangeNotification: String
@available(iOS 4.0, *)
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
  @discardableResult
  func hasMediaType(_ mediaType: String!) -> Bool
  func lockForConfiguration() throws
  func unlockForConfiguration()
  @discardableResult
  func supportsAVCaptureSessionPreset(_ preset: String!) -> Bool
  var isConnected: Bool { get }
  @available(iOS 7.0, *)
  var formats: [AnyObject]! { get }
  @available(iOS 7.0, *)
  var activeFormat: AVCaptureDeviceFormat!
  @available(iOS 7.0, *)
  var activeVideoMinFrameDuration: CMTime
  @available(iOS 7.0, *)
  var activeVideoMaxFrameDuration: CMTime
}
@available(iOS 4.0, *)
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
@available(iOS 4.0, *)
enum AVCaptureFlashMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case off
  case on
  case auto
}
extension AVCaptureDevice {
  var hasFlash: Bool { get }
  @available(iOS 5.0, *)
  var isFlashAvailable: Bool { get }
  @available(iOS 5.0, *)
  var isFlashActive: Bool { get }
  @discardableResult
  func isFlashModeSupported(_ flashMode: AVCaptureFlashMode) -> Bool
  var flashMode: AVCaptureFlashMode
}
@available(iOS 4.0, *)
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
  @available(iOS 5.0, *)
  var isTorchAvailable: Bool { get }
  @available(iOS 6.0, *)
  var isTorchActive: Bool { get }
  @available(iOS 5.0, *)
  var torchLevel: Float { get }
  @discardableResult
  func isTorchModeSupported(_ torchMode: AVCaptureTorchMode) -> Bool
  var torchMode: AVCaptureTorchMode
  @available(iOS 6.0, *)
  func setTorchModeOnWithLevel(_ torchLevel: Float) throws
}
@available(iOS 4.0, *)
enum AVCaptureFocusMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case locked
  case autoFocus
  case continuousAutoFocus
}
@available(iOS 7.0, *)
enum AVCaptureAutoFocusRangeRestriction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case near
  case far
}
extension AVCaptureDevice {
  @discardableResult
  func isFocusModeSupported(_ focusMode: AVCaptureFocusMode) -> Bool
  var focusMode: AVCaptureFocusMode
  var isFocusPointOfInterestSupported: Bool { get }
  var focusPointOfInterest: CGPoint
  var isAdjustingFocus: Bool { get }
  @available(iOS 7.0, *)
  var isAutoFocusRangeRestrictionSupported: Bool { get }
  @available(iOS 7.0, *)
  var autoFocusRangeRestriction: AVCaptureAutoFocusRangeRestriction
  @available(iOS 7.0, *)
  var isSmoothAutoFocusSupported: Bool { get }
  @available(iOS 7.0, *)
  var isSmoothAutoFocusEnabled: Bool
  @available(iOS 8.0, *)
  var lensPosition: Float { get }
  @available(iOS 8.0, *)
  func setFocusModeLockedWithLensPosition(_ lensPosition: Float, completionHandler handler: ((CMTime) -> Void)!)
}
@available(iOS 8.0, *)
let AVCaptureLensPositionCurrent: Float
@available(iOS 4.0, *)
enum AVCaptureExposureMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case locked
  case autoExpose
  case continuousAutoExposure
  @available(iOS 8.0, *)
  case custom
}
extension AVCaptureDevice {
  @discardableResult
  func isExposureModeSupported(_ exposureMode: AVCaptureExposureMode) -> Bool
  var exposureMode: AVCaptureExposureMode
  var isExposurePointOfInterestSupported: Bool { get }
  var exposurePointOfInterest: CGPoint
  var isAdjustingExposure: Bool { get }
  @available(iOS 8.0, *)
  var lensAperture: Float { get }
  @available(iOS 8.0, *)
  var exposureDuration: CMTime { get }
  @available(iOS 8.0, *)
  var iso: Float { get }
  @available(iOS 8.0, *)
  func setExposureModeCustomWithDuration(_ duration: CMTime, iso ISO: Float, completionHandler handler: ((CMTime) -> Void)!)
  @available(iOS 8.0, *)
  var exposureTargetOffset: Float { get }
  @available(iOS 8.0, *)
  var exposureTargetBias: Float { get }
  @available(iOS 8.0, *)
  var minExposureTargetBias: Float { get }
  @available(iOS 8.0, *)
  var maxExposureTargetBias: Float { get }
  @available(iOS 8.0, *)
  func setExposureTargetBias(_ bias: Float, completionHandler handler: ((CMTime) -> Void)!)
}
@available(iOS 8.0, *)
let AVCaptureExposureDurationCurrent: CMTime
@available(iOS 8.0, *)
let AVCaptureISOCurrent: Float
@available(iOS 8.0, *)
let AVCaptureExposureTargetBiasCurrent: Float
@available(iOS 4.0, *)
enum AVCaptureWhiteBalanceMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case locked
  case autoWhiteBalance
  case continuousAutoWhiteBalance
}
@available(iOS 8.0, *)
struct AVCaptureWhiteBalanceGains {
  var redGain: Float
  var greenGain: Float
  var blueGain: Float
  init()
  init(redGain redGain: Float, greenGain greenGain: Float, blueGain blueGain: Float)
}
@available(iOS 8.0, *)
struct AVCaptureWhiteBalanceChromaticityValues {
  var x: Float
  var y: Float
  init()
  init(x x: Float, y y: Float)
}
@available(iOS 8.0, *)
struct AVCaptureWhiteBalanceTemperatureAndTintValues {
  var temperature: Float
  var tint: Float
  init()
  init(temperature temperature: Float, tint tint: Float)
}
extension AVCaptureDevice {
  @discardableResult
  func isWhiteBalanceModeSupported(_ whiteBalanceMode: AVCaptureWhiteBalanceMode) -> Bool
  var whiteBalanceMode: AVCaptureWhiteBalanceMode
  var isAdjustingWhiteBalance: Bool { get }
  @available(iOS 8.0, *)
  var deviceWhiteBalanceGains: AVCaptureWhiteBalanceGains { get }
  @available(iOS 8.0, *)
  var grayWorldDeviceWhiteBalanceGains: AVCaptureWhiteBalanceGains { get }
  @available(iOS 8.0, *)
  var maxWhiteBalanceGain: Float { get }
  @available(iOS 8.0, *)
  func setWhiteBalanceModeLockedWithDeviceWhiteBalanceGains(_ whiteBalanceGains: AVCaptureWhiteBalanceGains, completionHandler handler: ((CMTime) -> Void)!)
  @available(iOS 8.0, *)
  @discardableResult
  func chromaticityValues(forDeviceWhiteBalanceGains whiteBalanceGains: AVCaptureWhiteBalanceGains) -> AVCaptureWhiteBalanceChromaticityValues
  @available(iOS 8.0, *)
  @discardableResult
  func deviceWhiteBalanceGains(for chromaticityValues: AVCaptureWhiteBalanceChromaticityValues) -> AVCaptureWhiteBalanceGains
  @available(iOS 8.0, *)
  @discardableResult
  func temperatureAndTintValues(forDeviceWhiteBalanceGains whiteBalanceGains: AVCaptureWhiteBalanceGains) -> AVCaptureWhiteBalanceTemperatureAndTintValues
  @available(iOS 8.0, *)
  @discardableResult
  func deviceWhiteBalanceGains(for tempAndTintValues: AVCaptureWhiteBalanceTemperatureAndTintValues) -> AVCaptureWhiteBalanceGains
}
@available(iOS 8.0, *)
let AVCaptureWhiteBalanceGainsCurrent: AVCaptureWhiteBalanceGains
extension AVCaptureDevice {
  @available(iOS 5.0, *)
  var isSubjectAreaChangeMonitoringEnabled: Bool
}
extension AVCaptureDevice {
  @available(iOS 6.0, *)
  var isLowLightBoostSupported: Bool { get }
  @available(iOS 6.0, *)
  var isLowLightBoostEnabled: Bool { get }
  @available(iOS 6.0, *)
  var automaticallyEnablesLowLightBoostWhenAvailable: Bool
}
extension AVCaptureDevice {
  @available(iOS 7.0, *)
  var videoZoomFactor: CGFloat
  @available(iOS 7.0, *)
  func ramp(toVideoZoomFactor factor: CGFloat, withRate rate: Float)
  @available(iOS 7.0, *)
  var isRampingVideoZoom: Bool { get }
  @available(iOS 7.0, *)
  func cancelVideoZoomRamp()
}
@available(iOS 7.0, *)
enum AVAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notDetermined
  case restricted
  case denied
  case authorized
}
extension AVCaptureDevice {
  @available(iOS 7.0, *)
  @discardableResult
  class func authorizationStatus(forMediaType mediaType: String!) -> AVAuthorizationStatus
  @available(iOS 7.0, *)
  class func requestAccess(forMediaType mediaType: String!, completionHandler handler: ((Bool) -> Void)!)
}
extension AVCaptureDevice {
  @available(iOS 8.0, *)
  var automaticallyAdjustsVideoHDREnabled: Bool
  @available(iOS 8.0, *)
  var isVideoHDREnabled: Bool
}
@available(iOS 7.0, *)
class AVFrameRateRange : NSObject {
  var minFrameRate: Float64 { get }
  var maxFrameRate: Float64 { get }
  var maxFrameDuration: CMTime { get }
  var minFrameDuration: CMTime { get }
}
@available(iOS 8.0, *)
enum AVCaptureVideoStabilizationMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case off
  case standard
  case cinematic
  case auto
}
@available(iOS 8.0, *)
enum AVCaptureAutoFocusSystem : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case contrastDetection
  case phaseDetection
}
@available(iOS 7.0, *)
class AVCaptureDeviceFormat : NSObject {
  var mediaType: String! { get }
  var formatDescription: CMFormatDescription! { get }
  var videoSupportedFrameRateRanges: [AnyObject]! { get }
  @available(iOS 7.0, *)
  var videoFieldOfView: Float { get }
  @available(iOS 7.0, *)
  var isVideoBinned: Bool { get }
  @available(iOS 8.0, *)
  @discardableResult
  func isVideoStabilizationModeSupported(_ videoStabilizationMode: AVCaptureVideoStabilizationMode) -> Bool
  @available(iOS, introduced: 7.0, deprecated: 8.0, message: "Use isVideoStabilizationModeSupported: instead.")
  var isVideoStabilizationSupported: Bool { get }
  @available(iOS 7.0, *)
  var videoMaxZoomFactor: CGFloat { get }
  @available(iOS 7.0, *)
  var videoZoomFactorUpscaleThreshold: CGFloat { get }
  @available(iOS 8.0, *)
  var minExposureDuration: CMTime { get }
  @available(iOS 8.0, *)
  var maxExposureDuration: CMTime { get }
  @available(iOS 8.0, *)
  var minISO: Float { get }
  @available(iOS 8.0, *)
  var maxISO: Float { get }
  @available(iOS 8.0, *)
  var isVideoHDRSupported: Bool { get }
  @available(iOS 8.0, *)
  var highResolutionStillImageDimensions: CMVideoDimensions { get }
  @available(iOS 8.0, *)
  var autoFocusSystem: AVCaptureAutoFocusSystem { get }
}
