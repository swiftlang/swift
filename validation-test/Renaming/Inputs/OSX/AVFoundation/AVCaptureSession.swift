
@available(OSX 10.7, *)
let AVCaptureSessionRuntimeErrorNotification: String
@available(OSX 10.7, *)
let AVCaptureSessionErrorKey: String
@available(OSX 10.7, *)
let AVCaptureSessionDidStartRunningNotification: String
@available(OSX 10.7, *)
let AVCaptureSessionDidStopRunningNotification: String
@available(OSX 10.7, *)
enum AVCaptureVideoOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case portrait
  case portraitUpsideDown
  case landscapeRight
  case landscapeLeft
}
@available(OSX 10.7, *)
let AVCaptureSessionPresetPhoto: String
@available(OSX 10.7, *)
let AVCaptureSessionPresetHigh: String
@available(OSX 10.7, *)
let AVCaptureSessionPresetMedium: String
@available(OSX 10.7, *)
let AVCaptureSessionPresetLow: String
@available(OSX 10.7, *)
let AVCaptureSessionPreset320x240: String
@available(OSX 10.7, *)
let AVCaptureSessionPreset352x288: String
@available(OSX 10.7, *)
let AVCaptureSessionPreset640x480: String
@available(OSX 10.7, *)
let AVCaptureSessionPreset960x540: String
@available(OSX 10.7, *)
let AVCaptureSessionPreset1280x720: String
@available(OSX 10.9, *)
let AVCaptureSessionPresetiFrame960x540: String
@available(OSX 10.9, *)
let AVCaptureSessionPresetiFrame1280x720: String
@available(OSX 10.7, *)
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
  @available(OSX 10.7, *)
  func addInputWithNoConnections(_ input: AVCaptureInput!)
  @available(OSX 10.7, *)
  func addOutputWithNoConnections(_ output: AVCaptureOutput!)
  @available(OSX 10.7, *)
  @discardableResult
  func canAdd(_ connection: AVCaptureConnection!) -> Bool
  @available(OSX 10.7, *)
  func add(_ connection: AVCaptureConnection!)
  @available(OSX 10.7, *)
  func remove(_ connection: AVCaptureConnection!)
  func beginConfiguration()
  func commitConfiguration()
  var isRunning: Bool { get }
  func startRunning()
  func stopRunning()
  @available(OSX 10.9, *)
  var masterClock: CMClock! { get }
}
@available(OSX 10.7, *)
enum AVVideoFieldMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case both
  case topOnly
  case bottomOnly
  case deinterlace
}
@available(OSX 10.7, *)
class AVCaptureConnection : NSObject {
  @available(OSX 10.7, *)
  init!(inputPorts ports: [AnyObject]!, output output: AVCaptureOutput!)
  @available(OSX 10.7, *)
  init!(inputPort port: AVCaptureInputPort!, videoPreviewLayer layer: AVCaptureVideoPreviewLayer!)
  var inputPorts: [AnyObject]! { get }
  var output: AVCaptureOutput! { get }
  @available(OSX 10.7, *)
  var videoPreviewLayer: AVCaptureVideoPreviewLayer! { get }
  var isEnabled: Bool
  var isActive: Bool { get }
  var audioChannels: [AnyObject]! { get }
  var isVideoMirroringSupported: Bool { get }
  var isVideoMirrored: Bool
  @available(OSX 10.7, *)
  var automaticallyAdjustsVideoMirroring: Bool
  var isVideoOrientationSupported: Bool { get }
  var videoOrientation: AVCaptureVideoOrientation
  @available(OSX 10.7, *)
  var isVideoFieldModeSupported: Bool { get }
  @available(OSX 10.7, *)
  var videoFieldMode: AVVideoFieldMode
  @available(OSX, introduced: 10.7, message: "Use AVCaptureDevice's activeFormat.videoSupportedFrameRateRanges instead.")
  var isVideoMinFrameDurationSupported: Bool { get }
  @available(OSX, introduced: 10.7, message: "Use AVCaptureDevice's activeVideoMinFrameDuration instead.")
  var videoMinFrameDuration: CMTime
  @available(OSX, introduced: 10.9, message: "Use AVCaptureDevice's activeFormat.videoSupportedFrameRateRanges instead.")
  var isVideoMaxFrameDurationSupported: Bool { get }
  @available(OSX, introduced: 10.9, message: "Use AVCaptureDevice's activeVideoMaxFrameDuration instead.")
  var videoMaxFrameDuration: CMTime
}
@available(OSX 10.7, *)
class AVCaptureAudioChannel : NSObject {
  var averagePowerLevel: Float { get }
  var peakHoldLevel: Float { get }
  @available(OSX 10.7, *)
  var volume: Float
  @available(OSX 10.7, *)
  var isEnabled: Bool
}
