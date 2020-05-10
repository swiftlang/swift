public protocol P {
  func myValue() -> Int64
}
extension Int64: P {
  public func myValue() -> Int64 {
    return self
  }
}
public func bar(_ x: Int64) -> some P {
  return x
}
public func foo(_ x: Int64) -> some P {
  if x > 0 {
  }
  return bar(x - 1)
}

public protocol CP : class { }

public class C : CP {
  public func myValue() -> Int64 {
    return 0
  }
}

public func returnC() -> some CP {
  return C()
}

public protocol P4 {
  associatedtype AT
  func foo(_ x: Int64) -> AT
}
public struct PA : P4 {
  public func foo(_ x: Int64)  -> some P {
    return Int64(x)
  }
}
extension PA {
  public func test() {
  }
}
public struct Foo {
  public var p : Int64 = 1
}
public struct Test : RandomAccessCollection {
  public struct Index : Comparable, Hashable {
    public var identifier: AnyHashable?
    public var offset: Int
    public static func < (lhs: Index, rhs: Index) -> Bool {
        return lhs.offset < rhs.offset
    }
    public func hash(into hasher: inout Hasher) {
    }
  }
  let foos: [Foo]
  let ids: [AnyHashable]
  func _index(atOffset n: Int) -> Index {
    return Index(identifier: ids.isEmpty ? nil : ids[n], offset: n)
  }
  public var startIndex: Index {
    return _index(atOffset: 0)
  }
  public var endIndex: Index {
    return Index(identifier: nil, offset: ids.endIndex)
  }
  public func index(after i: Index) -> Index {
    return _index(atOffset: i.offset + 1)
  }
  public func index(before i: Index) -> Index {
    return _index(atOffset: i.offset - 1)
  }
  public subscript(i: Index) -> some P {
    return foos[i.offset].p
  }
}
