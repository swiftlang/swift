// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source -remove-internal-decls %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
public class MyClass {
  public var name: String
  private var id: Int

  public init(name: String) {
    self.name = name
    self.id = 0
  }

  deinit {
    print("deinit")
  }

  private func privateHelper() -> Int { return 0 }

  internal func internalHelper() {}
}

public enum Direction {
  case north
  case south
  case east
  case west

  public func description() -> String {
    return "\(self)"
  }

  private func privateMethod() -> Int { return 0 }

  internal func internalHelper() -> Int { return 0 }
}

public struct Outer {
  public struct Inner {
    public var value: Int
    private var secret: Int
    internal var hidden: Int

    public func getValue() -> Int { return value }
    private func getSecret() -> Int { return secret }
    internal func getHidden() -> Int { return hidden }
  }
}

/// Struct without explicit init — default values are stripped when type
/// annotation is present.
public struct Config {
  public var timeout: Int = 30
  public var name: String = "default"
  private var counter: Int = 0
}

/// Struct with explicit init — default values can be stripped because
/// the memberwise init is suppressed.
public struct ConfigWithInit {
  public var timeout: Int = 30
  public var name: String = "default"
  private var counter: Int = 0
  internal var hidden: Int = 0

  public init(timeout: Int) {
    self.timeout = timeout
    self.name = "custom"
    self.counter = 1
    self.hidden = 0
  }
}

/// Class with explicit init — default values stripped.
public class Widget {
  public var label: String = "untitled"
  private var id: Int = 0

  public init(label: String) {
    self.label = label
  }
}

/// Class without explicit init — default values stripped.
public class DefaultWidget {
  public var label: String = "untitled"
  private var id: Int = 0
}

/// Struct with init but no type annotation on a property — default kept
/// because the type is inferred from the initializer.
public struct InferredType {
  public var name: String = "hello"
  public var count = 42

  public init(name: String) {
    self.name = name
  }
}

/// Lazy variables — initializer stripped when type annotation is present,
/// kept when type is inferred.
public class LazyProps {
  public lazy var typedLazy: Int = { 42 }()
  public lazy var inferredLazy = { 42 }()
  private lazy var privateLazy: String = { "hello" }()

  public init() {}
}

/// Tuple pattern binding — no type annotation, so default value is always
/// preserved regardless of whether the type has an explicit init.
public struct TupleBindingNoInit {
  public var (a, b) = (0, 0)
}

public struct TupleBindingWithInit {
  public var (a, b) = (0, 0)

  public init() {}
}

/// Protocols are kept regardless of access level.
public protocol PublicProto {
  func required()
}

internal protocol InternalProto {
  func required()
}

private protocol PrivateProto {
  func required()
}

fileprivate protocol FileprivateProto {
  func required()
}
//--- expected.swift
public class MyClass {
  public var name: String
  private var id: Int

  public init(name: String)

  deinit

  private func privateHelper() -> Int
}

public enum Direction {
  case north
  case south
  case east
  case west

  public func description() -> String

  private func privateMethod() -> Int
}

public struct Outer {
  public struct Inner {
    public var value: Int
    private var secret: Int

    public func getValue() -> Int
    private func getSecret() -> Int
  }
}

public struct Config {
  public var timeout: Int = 30
  public var name: String = "default"
  private var counter: Int = 0
}

public struct ConfigWithInit {
  public var timeout: Int
  public var name: String
  private var counter: Int

  public init(timeout: Int)
}

public class Widget {
  public var label: String
  private var id: Int

  public init(label: String)
}

public class DefaultWidget {
  public var label: String = "untitled"
  private var id: Int = 0
}

public struct InferredType {
  public var name: String
  public var count = 42

  public init(name: String)
}

public class LazyProps {
  public lazy var typedLazy: Int
  public lazy var inferredLazy = {
      42
  }()
  private lazy var privateLazy: String

  public init()
}

public struct TupleBindingNoInit {
  public var (a, b) = (0, 0)
}

public struct TupleBindingWithInit {
  public var (a, b) = (0, 0)

  public init()
}

public protocol PublicProto {
  func required()
}

internal protocol InternalProto {
  func required()
}

private protocol PrivateProto {
  func required()
}

fileprivate protocol FileprivateProto {
  func required()
}
