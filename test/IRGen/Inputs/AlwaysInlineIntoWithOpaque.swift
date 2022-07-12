public protocol P {
}

extension Int : P {
}

extension Double : P {
}

@_alwaysEmitIntoClient
public func testInlineWithOpaque() -> some P {
  if #available(macOS 9.0, *) {
    return 1
  }
  return 2.0
}
