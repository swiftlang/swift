// RUN: not %target-swift-frontend -emit-silgen %S/Inputs/library.swift -primary-file %S/main.swift

public func f(x: SomeStruct) {}

public protocol P {
  @_borrowed var x: Int { get set }
}

public struct HasAccessors : P {
  public var x: Int = 123
}
