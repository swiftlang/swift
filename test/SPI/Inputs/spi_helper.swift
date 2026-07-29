 /// Library defining SPI decls

public protocol PublicProto {
  associatedtype Assoc
}

public struct PublicType {
    public init() { }
}

public func publicFunc() { print("publicFunc") }

func internalFunc() {}

@_specialize(exported: true, spi: HelperSPI, where T == Int)
public func genericFunc<T>(_ t: T) { print(t) }

@_spi(HelperSPI) public func spiFunc() { print("spiFunc") }

@_spi(HelperSPI) public struct SPIStruct {
  @_spi(HelperSPI) public init() { print("SPIStruct.init") }
  @_spi(HelperSPI) public func spiMethod() { print("SPIStruct.spiMethod") }
  @_spi(HelperSPI) public var spiVar = "text"

  @_spi(HelperSPI) public var spiComputedVar: Int {
    get { return 42 }
    set {}
  }

  @_spi(HelperSPI) public var spiComputedVarInherit: Int {
    @_spi(HelperSPI) get { return 42 }
    @_spi(HelperSPI) set {}
  }
  @_spi(HelperSPI) public subscript(index: Int) -> String { return spiVar }

  public func spiInherit() {}
  @_spi(DifferentSPI) public func spiDontInherit() {}

  @_specialize(exported: true, spi: HelperSPI, where T == Int)
  @_spi(HelperSPI) public func genericFunc2<T>(_ t: T) { print(t) }
}

public extension SPIStruct {
  func spiExtensionHidden() {}
  @_spi(HelperSPI) func spiExtension() {}
}

@_spi(HelperSPI) public extension SPIStruct {
  func spiExtensionInherited() {}
}

@_spi(HelperSPI) public class SPIClass {
  @_spi(HelperSPI) public init() { print("SPIClass.init") }
  @_spi(HelperSPI) public func spiMethod() { print("SPIClass.spiMethod") }
  @_spi(HelperSPI) public var spiVar = "text"

  @_specialize(exported: true, spi: HelperSPI, where T == Int)
  @_spi(HelperSPI) public func genericFunc3<T>(_ t: T) { print(t) }
}

@_spi(HelperSPI) public enum SPIEnum {
  case A
  case B

  @_spi(HelperSPI) public init() {
    print("SPIEnum.init")
    self = .A
  }

  @_spi(HelperSPI) public func spiMethod() { print("SPIEnum.spiMethod") }

  @_specialize(exported: true, spi: HelperSPI, where T == Int)
  public func genericFunc4<T>(_ t: T) { print(t) }
}

public struct PublicStruct {
  public init() {}

  @_spi(HelperSPI) public init(alt_init: Int) { print("PublicStruct.init alt_init") }
  @_spi(HelperSPI) public func spiMethod() { print("PublicStruct.spiMethod") }
  @_spi(HelperSPI) public var spiVar = "text"

  @_specialize(exported: true, spi: HelperSPI, where T == Int)
  public func prespecializedMethod<T>(_ t: T) { print(t) }
}

@_spi(OtherSPI) public func otherApiFunc() {}

@_spi(HelperSPI) public struct ConformsToNormalProto: PublicProto {
  @_spi(HelperSPI) public typealias Assoc = Int
}

@_spi(HelperSPI) public struct IntLike: ExpressibleByIntegerLiteral, Equatable {
  @_spi(HelperSPI) public init(integerLiteral: Int) {}
}

public enum PublicEnum {
  case publicCase
  @_spi(HelperSPI) case spiCase
}

@_spi(HelperSPI) public func -(_ s1: PublicType, _ s2: PublicType) -> PublicType { s1 }
@_spi(HelperSPI) public func +(_ s1: PublicType, _ s2: PublicType) -> PublicType { s1 }

public protocol ProtoWithSPIRequirement {
  func publicReq()

  @_spi(HelperSPI) func spiReq()
}

extension ProtoWithSPIRequirement {
  @_spi(HelperSPI) public func spiReq() { }
}

public protocol ProtoWithSPIRequirementInOtherGroup {
  func publicReq()

  @_spi(OtherSPI) func spiReq()
}

extension ProtoWithSPIRequirementInOtherGroup {
  @_spi(OtherSPI) public func spiReq() { }
}

public protocol ProtoWithFutureSPIRequirement {
  func publicReq()

  @available(macOS 99, *)
  @available(iOS 99, *)
  @available(tvOS 99, *)
  @available(watchOS 99, *)
  @available(visionOS 99, *)
  @_spi(HelperSPI) func spiReq()
}

extension ProtoWithFutureSPIRequirement {
  @available(macOS 99, *)
  @available(iOS 99, *)
  @available(tvOS 99, *)
  @available(watchOS 99, *)
  @available(visionOS 99, *)
  @_spi(HelperSPI) public func spiReq() { }
}
