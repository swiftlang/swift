
enum CLError : Int {
  case locationUnknown
  case denied
  case network
  case headingFailure
  case regionMonitoringDenied
  case regionMonitoringFailure
  case regionMonitoringSetupDelayed
  case regionMonitoringResponseDelayed
  case geocodeFoundNoResult
  case geocodeFoundPartialResult
  case geocodeCanceled
  case deferredFailed
  case deferredNotUpdatingLocation
  case deferredAccuracyTooLow
  case deferredDistanceFiltered
  case deferredCanceled
  case rangingUnavailable
  case rangingFailure
}

extension CLError : _BridgedNSError {
}
