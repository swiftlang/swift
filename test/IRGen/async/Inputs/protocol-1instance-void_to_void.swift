import _Concurrency

public protocol Protokol {
  func protocolinstanceVoidToVoid() async
}

public struct Impl : Protokol {
  public init() {}
  public func protocolinstanceVoidToVoid() async {
    print(self)
  }
}

