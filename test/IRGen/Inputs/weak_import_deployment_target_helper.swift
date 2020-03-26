@available(macOS 10.50, *)
public func function() -> Int { return 0 }

@available(macOS 10.50, *)
public func hasDefaultArg(_: Int = function()) {}

@available(macOS 10.60, *)
public func availableFunction() -> Int { return 0 }

@available(macOS 10.60, *)
public func hasAvailableDefaultArg(_: Int = availableFunction()) {}
