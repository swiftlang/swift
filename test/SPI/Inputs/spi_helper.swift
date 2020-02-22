/// Library defining SPI decls

public protocol PublicProto {
  associatedtype Assoc
}

public func publicFunc() { print("publicFunc") }

func internalFunc() {}

@_spi(HelperSPI) public func spiFunc() { print("spiFunc") }

@_spi(HelperSPI) public class SPIStruct {
  @_spi(HelperSPI) public init() { print("SPIStruct.init") }
  @_spi(HelperSPI) public func spiMethod() { print("SPIStruct.spiMethod") }
  @_spi(HelperSPI) public var spiVar = "text"
  //@_spi(HelperSPI) public subscript(index: Int) -> String { return spiVar } // TODO SPI support for subscripts
}

@_spi(HelperSPI) public class SPIClass {
  @_spi(HelperSPI) public init() { print("SPIClass.init") }
  @_spi(HelperSPI) public func spiMethod() { print("SPIClass.spiMethod") }
  @_spi(HelperSPI) public var spiVar = "text"
}

@_spi(HelperSPI) public enum SPIEnum {
  case A
  case B

  @_spi(HelperSPI) public init() {
    print("SPIEnum.init")
    self = .A
  }

  @_spi(HelperSPI) public func spiMethod() { print("SPIEnum.spiMethod") }
}

public struct PublicStruct {
  public init() {}

  @_spi(HelperSPI) public init(alt_init: Int) { print("PublicStruct.init alt_init") }
  @_spi(HelperSPI) public func spiMethod() { print("PublicStruct.spiMethod") }
  @_spi(HelperSPI) public var spiVar = "text"
}

@_spi(OtherSPI) public func otherApiFunc() {}

@_spi(HelperSPI) public struct ConformsToNormalProto: PublicProto {
  @_spi(HelperSPI) public typealias Assoc = Int
}

@_spi(HelperSPI) public struct IntLike: ExpressibleByIntegerLiteral, Equatable {
  @_spi(HelperSPI) public init(integerLiteral: Int) {}
}
