// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
import Foundation
import Swift

public func publicFunc() {
  print("hello")
}

internal func internalFunc() {
  print("internal")
}

private func privateFunc() {
  print("private")
}

fileprivate func fileprivateFunc() {
  print("fileprivate")
}

public struct PublicStruct {
  public var x: Int
  private var y: Int

  public func publicMethod() {}
  internal func internalMethod() {}
  private func privateMethod() {}
}
//--- expected.swift
import Foundation
import Swift

public func publicFunc()

private func privateFunc()

public struct PublicStruct {
  public var x: Int
  private var y: Int

  public func publicMethod()
  private func privateMethod()
}
