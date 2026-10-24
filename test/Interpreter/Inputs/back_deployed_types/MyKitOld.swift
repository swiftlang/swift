// MyKit as it shipped in an older OS, before the back-deployed types existed.
// A protocol from MyKit itself, which isn't back-deployed. It was available
// before the back-deployed types were introduced.
public protocol Labeled {
  func label<T>(_ t: T) -> String
}

public func labelGenerically<L: Labeled>(_ l: L) -> String { l.label(0) }

public func myKitVersion() -> Int { 1 }
