public protocol P {}

public struct HasUnavailableConformance {}

@available(*, unavailable)
extension HasUnavailableConformance : P {}

public struct HasConditionallyAvailableConformance {}

@available(macOS 100, *)
extension HasConditionallyAvailableConformance : P {}

public struct HasAlwaysAvailableConformance {}

extension HasAlwaysAvailableConformance : P {}
