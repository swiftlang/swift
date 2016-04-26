
struct CMError : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var CMErrorNULL: CMError { get }
var CMErrorDeviceRequiresMovement: CMError { get }
var CMErrorTrueNorthNotAvailable: CMError { get }
var CMErrorUnknown: CMError { get }
var CMErrorMotionActivityNotAvailable: CMError { get }
var CMErrorMotionActivityNotAuthorized: CMError { get }
var CMErrorMotionActivityNotEntitled: CMError { get }
var CMErrorInvalidParameter: CMError { get }
var CMErrorInvalidAction: CMError { get }
var CMErrorNotAvailable: CMError { get }
var CMErrorNotEntitled: CMError { get }
var CMErrorNotAuthorized: CMError { get }
