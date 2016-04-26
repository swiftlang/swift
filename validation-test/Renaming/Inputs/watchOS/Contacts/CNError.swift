
@available(watchOS 2.0, *)
let CNErrorDomain: String
@available(watchOS 2.0, *)
enum CNErrorCode : Int {
  case communicationError
  case dataAccessError
  case authorizationDenied
  case recordDoesNotExist
  case insertedRecordAlreadyExists
  case containmentCycle
  case containmentScope
  case parentRecordDoesNotExist
  case validationMultipleErrors
  case validationTypeMismatch
  case validationConfigurationError
  case predicateInvalid
  case policyViolation
}

@available(OSX 10.11, iOS 9.0, *)
extension CNErrorCode : _BridgedNSError {
}
@available(watchOS 2.0, *)
let CNErrorUserInfoAffectedRecordsKey: String
@available(watchOS 2.0, *)
let CNErrorUserInfoAffectedRecordIdentifiersKey: String
@available(watchOS 2.0, *)
let CNErrorUserInfoValidationErrorsKey: String
@available(watchOS 2.0, *)
let CNErrorUserInfoKeyPathsKey: String
