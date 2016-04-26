
@available(OSX 10.11, *)
let CNErrorDomain: String
@available(OSX 10.11, *)
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
@available(OSX 10.11, *)
let CNErrorUserInfoAffectedRecordsKey: String
@available(OSX 10.11, *)
let CNErrorUserInfoAffectedRecordIdentifiersKey: String
@available(OSX 10.11, *)
let CNErrorUserInfoValidationErrorsKey: String
@available(OSX 10.11, *)
let CNErrorUserInfoKeyPathsKey: String
