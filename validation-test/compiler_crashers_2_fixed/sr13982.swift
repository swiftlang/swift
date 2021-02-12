// RUN: not %target-swift-frontend -emit-ir %s

public protocol ProtoDelegate where Self.Manager.Delegate: Self {
  associatedtype Manager: ProtoManager
  func bind(to: Manager)
}

public protocol ProtoManager where Self.Delegate.Manager: Self {
  associatedtype Delegate: ProtoDelegate
  var name: String { get }
}
