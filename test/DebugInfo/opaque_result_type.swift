// RUN: %target-swift-frontend -emit-ir -g %s -target %target-swift-5.1-abi-triple

public protocol P {
  associatedtype Horse
}

public protocol Feed {}

public struct Hay : Feed {}

public func hasOpaqueResult<T : P>(_: T.Type, _: T.Horse) -> some Feed {
  return Hay()
}


