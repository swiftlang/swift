public class PublicClass {
  public func method() {
  }

  public init() {
  }
}

public class PublicSubclass: PublicClass {
  public override func method() {
  }
}

public protocol PublicProtocol {
  var publicStruct: PublicStruct { get }
}

public struct PublicStruct {
  public init() {}
}

extension PublicStruct: PublicProtocol {
  public var publicStruct: PublicStruct { return self }
}

public enum PublicEnum: PublicProtocol {
  case caseOne
  case caseTwo

  public var publicStruct: PublicStruct { return PublicStruct() }
}
