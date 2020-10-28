public class Empty {}

public class TwoInts {
  public var x, y : Int
  
  public init(a : Int, b : Int) {
    x = a
    y = b
  }
}

public class ComputedProperty {
  public var value : Int {
    get {
      let result = 0
      return result
    }
    set(newVal) {
      // completely ignore it!
    }
  }
}

public protocol Prot1 { }
public protocol Prot2 : Prot1 { }
public protocol Prot3 { }

public class C2 { }

extension C2 : Prot3, Prot1, Prot2 { }

public func globalFunc() {}

private func SECRET() {}

extension C2 {
  public func publicFunc() {}
}

// Don't record extensions with nothing to index.
extension C2 {
  internal func SECRET() {}
  private func SECRET1() {}
  fileprivate func SECRET2() {}
}

internal protocol InternalProto {}
extension C2: InternalProto {}
