// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source -remove-internal-decls %t/input.swift > %t/output.swift
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

/// static let/var in extension — initializers must be preserved.
public extension MyType {
  static let defaultName: String = "MyType"
  static var mutableDefault: Int = 0
}

/// private extension — members default to private and should be kept.
private extension MyType {
  func privateExtMethod() {
    print("private ext")
  }

  var privateExtComputed: Int {
    return 42
  }

  static let privateExtConst: String = "private"
}

/// internal extension — members default to internal and should be removed.
internal extension MyType {
  func internalExtMethod() {
    print("internal ext")
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

public extension MyType {
  static let defaultName: String = "MyType"
  static var mutableDefault: Int = 0
}

private extension MyType {
  func privateExtMethod()

  var privateExtComputed: Int {
      get
  }

  static let privateExtConst: String = "private"
}
