// Test how SPI affects access control in protocol conformance lookup.

// RUN: %target-build-swift %s -swift-version 5

// rdar://61043406

@_spi(Private)
public struct SPIEquatable : Equatable {
  public static func ==(lhs: SPIEquatable, rhs: SPIEquatable) -> Bool {
    return true
  }
}

// rdar://61987739

@_spi(Private)
public struct SPIStruct {}

public protocol PublicProto {
  func thingOne()

  @_spi(Private)
  func thingTwo(_ data: SPIStruct)
}

extension PublicProto {
  @_spi(Private)
  public func thingTwo(_ data: SPIStruct) {
    // default implementation
  }
}

fileprivate struct FilePrivateStruct: PublicProto {
  func thingOne() {
    // OK
  }
}
