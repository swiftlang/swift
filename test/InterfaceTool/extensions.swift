// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
public struct MyType {}

public extension MyType {
  func publicExtMethod() {
    print("public ext")
  }

  internal func internalInPublicExt() {}
}

extension MyType {
  func implicitInternalMethod() {
    print("implicit internal")
  }
}

extension MyType: CustomStringConvertible {
  public var description: String {
    return "MyType"
  }
}
//--- expected.swift
public struct MyType {
}

public extension MyType {
  func publicExtMethod()
}

extension MyType: CustomStringConvertible {
  public var description: String {
      get
  }
}
