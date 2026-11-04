import Lib

@available(macOS 10.15, *)
public func use(_ a: Derived, _ b: Derived) -> Bool {
  a == b || a.hashValue == b.hashValue
}

@available(macOS 10.15, *)
public func useAny(_ a: Derived) -> AnyHashable { AnyHashable(a) }

@available(macOS 10.15, *)
public func useKind(_ a: Kind) -> Bool { a == .a }
