
@available(iOS 4.0, *)
let AVCaptureSessionRuntimeErrorNotification: String
@available(iOS 4.0, *)
let AVCaptureSessionErrorKey: String
@available(iOS 4.0, *)
let AVCaptureSessionDidStartRunningNotification: String
@available(iOS 4.0, *)
let AVCaptureSessionDidStopRunningNotification: String
@available(iOS 4.0, *)
let AVCaptureSessionWasInterruptedNotification: String
@available(iOS 9.0, *)
enum AVCaptureSessionInterruptionReason : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case videoDeviceNotAvailableInBackground
  case audioDeviceInUseByAnotherClient
  case videoDeviceInUseByAnotherClient
  case videoDeviceNotAvailableWithMultipleForegroundApps
}
@available(iOS 9.0, *)
let AVCaptureSessionInterruptionReasonKey: String
@available(iOS 4.0, *)
let AVCaptureSessionInterruptionEndedNotification: String
@available(iOS 4.0, *)
enum AVCaptureVideoOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case portrait
  case portraitUpsideDown
  case landscapeRight
  case landscapeLeft
}
@available(iOS 4.0, *)
let AVCaptureSessionPresetPhoto: String
@available(iOS 4.0, *)
let AVCaptureSessionPresetHigh: String
@available(iOS 4.0, *)
let AVCaptureSessionPresetMedium: String
@available(iOS 4.0, *)
let AVCaptureSessionPresetLow: String
@available(iOS 5.0, *)
let AVCaptureSessionPreset352x288: String
@available(iOS 4.0, *)
let AVCaptureSessionPreset640x480: String
@available(iOS 4.0, *)
let AVCaptureSessionPreset1280x720: String
@available(iOS 5.0, *)
let AVCaptureSessionPreset1920x1080: String
@available(iOS 9.0, *)
let AVCaptureSessionPreset3840x2160: String
@available(iOS 5.0, *)
let AVCaptureSessionPresetiFrame960x540: String
@available(iOS 5.0, *)
let AVCaptureSessionPresetiFrame1280x720: String
@available(iOS 7.0, *)
let AVCaptureSessionPresetInputPriority: String
@available(iOS 4.0, *)
class AVCaptureSession : NSObject {
  @discardableResult
  func canSetSessionPreset(_ preset: String!) -> Bool
  var sessionPreset: String!
  var inputs: [AnyObject]! { get }
  @discardableResult
  func canAddInput(_ input: AVCaptureInput!) -> Bool
  func addInput(_ input: AVCaptureInput!)
  func removeInput(_ input: AVCaptureInput!)
  var outputs: [AnyObject]! { get }
  @discardableResult
  func canAddOutput(_ output: AVCaptureOutput!) -> Bool
  func addOutput(_ output: AVCaptureOutput!)
  func removeOutput(_ output: AVCaptureOutput!)
  @available(iOS 8.0, *)
  func addInputWithNoConnections(_ input: AVCaptureInput!)
  @available(iOS 8.0, *)
  func addOutputWithNoConnections(_ output: AVCaptureOutput!)
  @available(iOS 8.0, *)
  @discardableResult
  func canAdd(_ connection: AVCaptureConnection!) -> Bool
  @available(iOS 8.0, *)
  func add(_ connection: AVCaptureConnection!)
  @available(iOS 8.0, *)
  func remove(_ connection: AVCaptureConnection!)
  func beginConfiguration()
  func commitConfiguration()
  var isRunning: Bool { get }
  @available(iOS 4.0, *)
  var isInterrupted: Bool { get }
  @available(iOS 7.0, *)
  var usesApplicationAudioSession: Bool
  @available(iOS 7.0, *)
  var automaticallyConfiguresApplicationAudioSession: Bool
  func startRunning()
  func stopRunning()
  @available(iOS 7.0, *)
  var masterClock: CMClock! { get }
}
@available(iOS 4.0, *)
class AVCaptureConnection : NSObject {
  @available(iOS 8.0, *)
  init!(inputPorts ports: [AnyObject]!, output output: AVCaptureOutput!)
  @available(iOS 8.0, *)
  init!(inputPort port: AVCaptureInputPort!, videoPreviewLayer layer: AVCaptureVideoPreviewLayer!)
  var inputPorts: [AnyObject]! { get }
  var output: AVCaptureOutput! { get }
  @available(iOS 6.0, *)
  var videoPreviewLayer: AVCaptureVideoPreviewLayer! { get }
  var isEnabled: Bool
  var isActive: Bool { get }
  var audioChannels: [AnyObject]! { get }
  var isVideoMirroringSupported: Bool { get }
  var isVideoMirrored: Bool
  @available(iOS 6.0, *)
  var automaticallyAdjustsVideoMirroring: Bool
  var isVideoOrientationSupported: Bool { get }
  var videoOrientation: AVCaptureVideoOrientation
  @available(iOS 5.0, *)
  var videoMaxScaleAndCropFactor: CGFloat { get }
  @available(iOS 5.0, *)
  var videoScaleAndCropFactor: CGFloat
  @available(iOS 8.0, *)
  var preferredVideoStabilizationMode: AVCaptureVideoStabilizationMode
  @available(iOS 8.0, *)
  var activeVideoStabilizationMode: AVCaptureVideoStabilizationMode { get }
  @available(iOS 6.0, *)
  var isVideoStabilizationSupported: Bool { get }
  @available(iOS, introduced: 6.0, deprecated: 8.0, message: "Use activeVideoStabilizationMode instead.")
  var isVideoStabilizationEnabled: Bool { get }
  @available(iOS, introduced: 6.0, deprecated: 8.0, message: "Use preferredVideoStabilizationMode instead.")
  var enablesVideoStabilizationWhenAvailable: Bool
}
@available(iOS 4.0, *)
class AVCaptureAudioChannel : NSObject {
  var averagePowerLevel: Float { get }
  var peakHoldLevel: Float { get }
}
