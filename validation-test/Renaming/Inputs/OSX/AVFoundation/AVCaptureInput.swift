
@available(OSX 10.7, *)
class AVCaptureInput : NSObject {
  var ports: [AnyObject]! { get }
}
@available(OSX 10.7, *)
let AVCaptureInputPortFormatDescriptionDidChangeNotification: String
@available(OSX 10.7, *)
class AVCaptureInputPort : NSObject {
  var input: AVCaptureInput! { get }
  var mediaType: String! { get }
  var formatDescription: CMFormatDescription! { get }
  var isEnabled: Bool
  @available(OSX 10.9, *)
  var clock: CMClock! { get }
}
@available(OSX 10.7, *)
class AVCaptureDeviceInput : AVCaptureInput {
  init(device device: AVCaptureDevice!) throws
  var device: AVCaptureDevice! { get }
}
@available(OSX 10.7, *)
class AVCaptureScreenInput : AVCaptureInput {
  init!(displayID displayID: CGDirectDisplayID)
  var minFrameDuration: CMTime
  var cropRect: CGRect
  var scaleFactor: CGFloat
  var capturesMouseClicks: Bool
  @available(OSX 10.8, *)
  var capturesCursor: Bool
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  var removesDuplicateFrames: Bool
}
