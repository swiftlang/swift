

public func isMacOSAfterFarFutureOriOSAfterFarFuture() -> Bool {
  if #available(macOS 50, iOS 50.0, *) {
    return true
  }

  return false
}

public func isMacOSAfterFarFutureOriOSAfterDistantPast() -> Bool {
  if #available(macOS 50, iOS 1.0, *) {
    return true
  }

  return false
}

public func isMacOSAfterDistantPastOriOSAfterFarFuture() -> Bool {
  if #available(macOS 1.0, iOS 50.0, *) {
    return true
  }

  return false
}

public func isMacOSAfterDistantPastOriOSAfterDistantPast() -> Bool {
  if #available(macOS 1.0, iOS 1.0, *) {
    return true
  }

  return false
}
