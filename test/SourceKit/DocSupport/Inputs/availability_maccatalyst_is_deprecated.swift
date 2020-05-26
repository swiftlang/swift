/// Some doc
@available(iOS, deprecated)
public func isAlwaysDeprecated_iOS() {}

@available(macCatalyst, deprecated)
public func isAlwaysDeprecated_catalyst() {}

@available(iOS, deprecated: 20.0)
public func deprecatedInFutureVersion_iOS() {}

@available(macCatalyst, deprecated: 20.0)
public func deprecatedInFutureVersion_catalyst() {}

@available(iOS, deprecated: 1.0)
public func deprecatedInPastVersion_iOS() {}

@available(macCatalyst, deprecated: 1.0)
public func deprecatedInPastVersion_catalyst() {}