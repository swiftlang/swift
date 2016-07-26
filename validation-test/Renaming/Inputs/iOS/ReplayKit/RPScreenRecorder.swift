
class RPScreenRecorder : NSObject {
  @discardableResult
  class func shared() -> RPScreenRecorder
  func startRecording(withMicrophoneEnabled microphoneEnabled: Bool, handler handler: ((NSError?) -> Void)? = nil)
  func stopRecording(handler handler: ((RPPreviewViewController?, NSError?) -> Void)? = nil)
  func discardRecording(handler handler: () -> Void)
  weak var delegate: @sil_weak RPScreenRecorderDelegate?
  var isRecording: Bool { get }
  var isMicrophoneEnabled: Bool { get }
  var isAvailable: Bool { get }
}
@available(iOS 9.0, *)
protocol RPScreenRecorderDelegate : NSObjectProtocol {
  optional func screenRecorder(_ screenRecorder: RPScreenRecorder, didStopRecordingWithError error: NSError, previewViewController previewViewController: RPPreviewViewController?)
  optional func screenRecorderDidChangeAvailability(_ screenRecorder: RPScreenRecorder)
}
