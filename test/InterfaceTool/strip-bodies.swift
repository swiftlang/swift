// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
import Foundation

public func publicFunc() {
  print("hello")
}

internal func internalFunc() {
  print("internal")
}

func implicitInternalFunc() {
  print("implicit")
}

private func privateFunc() {
  print("private")
}

fileprivate func fileprivateFunc() {
  print("fileprivate")
}

public struct PublicStruct {
  public var x: Int
  internal var y: Int
  fileprivate var z: Int

  public func publicMethod() { print("pub") }
  internal func internalMethod() { print("int") }
  fileprivate func fileprivateMethod() { print("fp") }
}

internal struct InternalStruct {
  var value: Int
  func doSomething() { print("something") }
}

fileprivate struct FileprivateStruct {
  var value: Int
}

@inlinable public func inlinableFunc() {
  let x = 1
  print(x)
}

@inlinable internal func inlinableInternalFunc() {
  print("inlinable internal")
}

public class MyClass {
  public init() { print("init") }
  deinit { print("deinit") }

  public var computed: Int {
    get { return 42 }
    set { print(newValue) }
  }

  public subscript(index: Int) -> Int {
    get { return index }
    set { print(newValue) }
  }
}
//--- expected.swift
import Foundation

public func publicFunc()

internal func internalFunc()

func implicitInternalFunc()

private func privateFunc()

fileprivate func fileprivateFunc()

public struct PublicStruct {
  public var x: Int
  internal var y: Int
  fileprivate var z: Int

  public func publicMethod()
  internal func internalMethod()
  fileprivate func fileprivateMethod()
}

internal struct InternalStruct {
  var value: Int
  func doSomething()
}

fileprivate struct FileprivateStruct {
  var value: Int
}

@inlinable public func inlinableFunc() {
  let x = 1
  print(x)
}

@inlinable internal func inlinableInternalFunc() {
  print("inlinable internal")
}

public class MyClass {
  public init()
  deinit

  public var computed: Int {
    get
    set
  }

  public subscript(index: Int) -> Int {
    get
    set
  }
}
