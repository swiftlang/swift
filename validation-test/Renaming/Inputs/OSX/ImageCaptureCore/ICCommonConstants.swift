
enum ICEXIFOrientationType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case orientation1
  case orientation2
  case orientation3
  case orientation4
  case orientation5
  case orientation6
  case orientation7
  case orientation8
}
enum ICReturnCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case success
  case invalidParam
  case communicationTimedOut
  case scanOperationCanceled
  case scannerInUseByLocalUser
  case scannerInUseByRemoteUser
  case deviceFailedToOpenSession
  case deviceFailedToCloseSession
  case scannerFailedToSelectFunctionalUnit
  case scannerFailedToCompleteOverviewScan
  case scannerFailedToCompleteScan
  case receivedUnsolicitedScannerStatusInfo
  case receivedUnsolicitedScannerErrorInfo
  case downloadFailed
  case uploadFailed
  case failedToCompletePassThroughCommand
  case downloadCanceled
  case failedToEnabeTethering
  case failedToDisabeTethering
  case failedToCompleteSendMessageRequest
  case deleteFilesFailed
  case deleteFilesCanceled
  case deviceIsPasscodeLocked
  case deviceFailedToTakePicture
  case deviceSoftwareNotInstalled
  case deviceSoftwareIsBeingInstalled
  case deviceSoftwareInstallationCompleted
  case deviceSoftwareInstallationCanceled
  case deviceSoftwareInstallationFailed
  case deviceSoftwareNotAvailable
  case deviceCouldNotPair
  case deviceCouldNotUnpair
  case deviceNeedsCredentials
}
