// The library half of
// test/SILOptimizer/constant_propagation_availability_maccatalyst_zippered.swift.

@available(macOS 10.52, iOS 50.0, *)
public func newerOnBoth() -> Int { return 1 }

@available(macOS 10.52, *)
public func newerOnMacOS() -> Int { return 2 }

@available(iOS 50.0, *)
public func newerOnMacCatalyst() -> Int { return 3 }

public func older() -> Int { return 0 }

@inlinable
public func queryBothBelow() -> Int {
  if #available(macOS 10.52, iOS 50.0, *) {
    return newerOnBoth()
  }
  return older()
}

@inlinable
public func queryBothAboveMacOS() -> Int {
  if #available(macOS 10.54, iOS 50.0, *) {
    return newerOnBoth()
  }
  return older()
}

@inlinable
public func queryBothAboveMacCatalyst() -> Int {
  if #available(macOS 10.52, iOS 52.0, *) {
    return newerOnBoth()
  }
  return older()
}

@inlinable
public func queryMacOSBelow() -> Int {
  if #available(macOS 10.52, *) {
    return newerOnMacOS()
  }
  return older()
}

@inlinable
public func queryMacOSAbove() -> Int {
  if #available(macOS 10.54, *) {
    return newerOnMacOS()
  }
  return older()
}

@inlinable
public func queryMacCatalystBelow() -> Int {
  if #available(iOS 50.0, *) {
    return newerOnMacCatalyst()
  }
  return older()
}

@inlinable
public func queryMacCatalystAbove() -> Int {
  if #available(iOS 52.0, *) {
    return newerOnMacCatalyst()
  }
  return older()
}
