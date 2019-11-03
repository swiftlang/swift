public protocol Proto {
  associatedtype Assoc : Proto
  var value: Assoc { get }
}

extension Never : Proto {}

extension Never {
    public typealias Assoc = Never

    public var value: Never {
        switch self {}
    }
}
protocol PrimitiveProto : Proto {}

extension PrimitiveProto {
    public var value: Never { valueError() }
}

extension Proto {
    func valueError() -> Never {
        fatalError("value() should not be called on \(Self.self).")
    }
}

public struct EmptyProto : PrimitiveProto {
  public init() {}
}

struct M<Content: Proto> : Proto {
  var t: Content

  init(_ t: Content) {
    self.t = t
  }

  var value: some Proto {
    return t.value
  }
}

public struct Group<T> {
  var t: T

  public init(_ t: T) {
    self.t = t
  }
}

extension Group : Proto, PrimitiveProto where T : Proto {
  public typealias Assoc = Never
}

public struct Choice<T, V>{
  var v: V

  public init(_ t: T, _ v: V) {
    self.v = v
  }
}

extension Choice : Proto where T: Proto, V: Proto {
  public var value: some Proto {
    return v.value
  }
}

extension Proto {
  public func add() -> some Proto  {
    return M(self)
  }
}
