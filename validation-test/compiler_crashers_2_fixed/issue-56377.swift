// RUN: not %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/56377

public protocol ProtoDelegate where Self.Manager.Delegate: Self {
  associatedtype Manager: ProtoManager
  func bind(to: Manager)
}

public protocol ProtoManager where Self.Delegate.Manager: Self {
  associatedtype Delegate: ProtoDelegate
  var name: String { get }
}
