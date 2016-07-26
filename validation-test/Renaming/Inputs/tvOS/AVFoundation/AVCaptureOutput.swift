
extension AVCaptureStillImageOutput {
  @available(tvOS 8.0, *)
  var maxBracketedCaptureStillImageCount: Int { get }
  @available(tvOS 9.0, *)
  var isLensStabilizationDuringBracketedCaptureSupported: Bool { get }
  @available(tvOS 9.0, *)
  var isLensStabilizationDuringBracketedCaptureEnabled: Bool
  @available(tvOS 8.0, *)
  func prepareToCaptureStillImageBracket(from connection: AVCaptureConnection!, withSettingsArray settings: [AnyObject]!, completionHandler handler: ((Bool, NSError!) -> Void)!)
  @available(tvOS 8.0, *)
  func captureStillImageBracketAsynchronously(from connection: AVCaptureConnection!, withSettingsArray settings: [AnyObject]!, completionHandler handler: ((CMSampleBuffer!, AVCaptureBracketedStillImageSettings!, NSError!) -> Void)!)
}
