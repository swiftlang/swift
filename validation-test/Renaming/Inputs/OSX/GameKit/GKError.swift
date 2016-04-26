
let GKErrorDomain: String
enum GKErrorCode : Int {
  case unknown
  case cancelled
  case communicationsFailure
  case userDenied
  case invalidCredentials
  case notAuthenticated
  case authenticationInProgress
  case invalidPlayer
  case scoreNotSet
  case parentalControlsBlocked
  case playerStatusExceedsMaximumLength
  case playerStatusInvalid
  case matchRequestInvalid
  case underage
  case gameUnrecognized
  case notSupported
  case invalidParameter
  case unexpectedConnection
  case challengeInvalid
  case turnBasedMatchDataTooLarge
  case turnBasedTooManySessions
  case turnBasedInvalidParticipant
  case turnBasedInvalidTurn
  case turnBasedInvalidState
  case invitationsDisabled
  case playerPhotoFailure
  case ubiquityContainerUnavailable
}

extension GKErrorCode : _BridgedNSError {
}
