class CameraController
{
  private func checkDeviceAuthorizationStatus()
  {
    AVCaptureDevice.requestAccessForMediaType(AVMediaTypeVideo, completionHandler: { (granted) -> Void in
}
