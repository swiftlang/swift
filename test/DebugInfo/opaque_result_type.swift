// RUN: %target-swift-frontend -emit-ir -g %s -disable-availability-checking

public protocol P {
  associatedtype Horse
}

public protocol Feed {}

public struct Hay : Feed {}

public func hasOpaqueResult<T : P>(_: T.Type, _: T.Horse) -> some Feed {
  return Hay()
}


