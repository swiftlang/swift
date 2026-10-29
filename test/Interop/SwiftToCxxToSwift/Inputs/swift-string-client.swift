import SwiftStringRoundTrip

@_alwaysEmitIntoClient
public func serializedRoundTrip(_ value: Swift.String) -> Swift.String {
  var copy = value
  mutateString(&copy)
  return echoString(borrowString(copy))
}
