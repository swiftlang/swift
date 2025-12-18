// RUN: not %target-swift-frontend -typecheck %s

public struct PeakMemoryUsage {
  public let rawValue: UInt64

  @_spi( init(rawValue: UInt64) {
    self.rawValue = rawValue
  }
}
