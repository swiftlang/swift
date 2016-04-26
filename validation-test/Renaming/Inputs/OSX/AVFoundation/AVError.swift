
@available(OSX 10.7, *)
let AVFoundationErrorDomain: String
@available(OSX 10.7, *)
let AVErrorDeviceKey: String
@available(OSX 10.7, *)
let AVErrorTimeKey: String
@available(OSX 10.7, *)
let AVErrorFileSizeKey: String
@available(OSX 10.7, *)
let AVErrorPIDKey: String
@available(OSX 10.7, *)
let AVErrorRecordingSuccessfullyFinishedKey: String
@available(OSX 10.7, *)
let AVErrorMediaTypeKey: String
@available(OSX 10.7, *)
let AVErrorMediaSubTypeKey: String
@available(OSX 10.10, *)
let AVErrorPresentationTimeStampKey: String
@available(OSX 10.10, *)
let AVErrorPersistentTrackIDKey: String
@available(OSX 10.10, *)
let AVErrorFileTypeKey: String
@available(OSX 10.7, *)
let AVErrorDiscontinuityFlagsKey: String
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
  case operationNotSupportedForAsset
  case decoderTemporarilyUnavailable
  case encoderTemporarilyUnavailable
  case invalidVideoComposition
  case referenceForbiddenByReferencePolicy
  case invalidOutputURLPathExtension
  case screenCaptureFailed
  case displayWasDisabled
  case torchLevelUnavailable
  case incompatibleAsset
  case failedToLoadMediaData
  case serverIncorrectlyConfigured
  case applicationIsNotAuthorizedToUseDevice
  @available(OSX 10.10, *)
  case failedToParse
  @available(OSX 10.10, *)
  case fileTypeDoesNotSupportSampleReferences
  @available(OSX 10.10, *)
  case undecodableMediaData
  @available(OSX 10.10, *)
  case airPlayControllerRequiresInternet
  @available(OSX 10.10, *)
  case airPlayReceiverRequiresInternet
  @available(OSX 10.11, *)
  case videoCompositorFailed
  @available(OSX 10.11, *)
  case createContentKeyRequestFailed
}

extension AVError : _BridgedNSError {
}
