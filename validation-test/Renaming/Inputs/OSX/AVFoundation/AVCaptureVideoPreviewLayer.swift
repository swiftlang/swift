
@available(OSX 10.7, *)
class AVCaptureVideoPreviewLayer : CALayer {
  init!(session session: AVCaptureSession!)
  @available(OSX 10.7, *)
  init!(sessionWithNoConnection session: AVCaptureSession!)
  var session: AVCaptureSession!
  @available(OSX 10.7, *)
  func setSessionWithNoConnection(_ session: AVCaptureSession!)
  @available(OSX 10.7, *)
  var connection: AVCaptureConnection! { get }
  var videoGravity: String!
}
