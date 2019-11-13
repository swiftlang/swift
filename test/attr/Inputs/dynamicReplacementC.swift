import A

public extension TheReplaceables {
  dynamic var property1: Int { return 0 }
  dynamic var property2: String { return "" }

  dynamic subscript (i: Int) -> Int { return 0 }
  dynamic subscript (s: String) -> String { return "" }
}

public class K {
  public init(i: Int) {}
  public convenience init(c : Int) { self.init(i : c) }
  public final func finalFunction() {}
}


public protocol P {
  var v: Int { get }
  subscript(i: Int) -> Int { get }
  func f()
}

extension P {
  public var v: Int { return 0 }

  public subscript(i: Int) -> Int {
    get {
      return 0
    }
  }

  public func f() {
  }
}
