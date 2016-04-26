
@available(iOS 4.0, *)
let AVFoundationErrorDomain: String
@available(iOS 4.0, *)
let AVErrorDeviceKey: String
@available(iOS 4.0, *)
let AVErrorTimeKey: String
@available(iOS 4.0, *)
let AVErrorFileSizeKey: String
@available(iOS 4.0, *)
let AVErrorPIDKey: String
@available(iOS 4.0, *)
let AVErrorRecordingSuccessfullyFinishedKey: String
@available(iOS 4.3, *)
let AVErrorMediaTypeKey: String
@available(iOS 4.3, *)
let AVErrorMediaSubTypeKey: String
@available(iOS 8.0, *)
let AVErrorPresentationTimeStampKey: String
@available(iOS 8.0, *)
let AVErrorPersistentTrackIDKey: String
@available(iOS 8.0, *)
let AVErrorFileTypeKey: String
enum AVError : Int {
  case unknown
  case outOfMemory
  case sessionNotRunning
  case deviceAlreadyUsedByAnotherSession
  case noDataCaptured
  case sessionConfigurationChanged
  case diskFull
  case deviceWasDisconnected
  case mediaChanged
  case maximumDurationReached
  case maximumFileSizeReached
  case mediaDiscontinuity
  case maximumNumberOfSamplesForFileFormatReached
  case deviceNotConnected
  case deviceInUseByAnotherApplication
  case deviceLockedForConfigurationByAnotherProcess
  case sessionWasInterrupted
  case mediaServicesWereReset
  case exportFailed
  case decodeFailed
  case invalidSourceMedia
  case fileAlreadyExists
  case compositionTrackSegmentsNotContiguous
  case invalidCompositionTrackSegmentDuration
  case invalidCompositionTrackSegmentSourceStartTime
  case invalidCompositionTrackSegmentSourceDuration
  case fileFormatNotRecognized
  case fileFailedToParse
  case maximumStillImageCaptureRequestsExceeded
  case contentIsProtected
  case noImageAtTime
  case decoderNotFound
  case encoderNotFound
  case contentIsNotAuthorized
  case applicationIsNotAuthorized
  @available(iOS, introduced: 4.3, deprecated: 9.0, message: "AVCaptureSession no longer produces an AVCaptureSessionRuntimeErrorNotification with this error. See AVCaptureSessionInterruptionReasonVideoDeviceNotAvailableInBackground.")
  case deviceIsNotAvailableInBackground
  case operationNotSupportedForAsset
  case decoderTemporarilyUnavailable
  case encoderTemporarilyUnavailable
  case invalidVideoComposition
  case referenceForbiddenByReferencePolicy
  case invalidOutputURLPathExtension
  case screenCaptureFailed
  case displayWasDisabled
  case torchLevelUnavailable
  case operationInterrupted
  case incompatibleAsset
  case failedToLoadMediaData
  case serverIncorrectlyConfigured
  case applicationIsNotAuthorizedToUseDevice
  @available(iOS 8.0, *)
  case failedToParse
  @available(iOS 8.0, *)
  case fileTypeDoesNotSupportSampleReferences
  @available(iOS 8.0, *)
  case undecodableMediaData
  @available(iOS 8.3, *)
  case airPlayControllerRequiresInternet
  @available(iOS 8.3, *)
  case airPlayReceiverRequiresInternet
  @available(iOS 9.0, *)
  case videoCompositorFailed
  @available(iOS 9.0, *)
  case recordingAlreadyInProgress
}

extension AVError : _BridgedNSError {
}
