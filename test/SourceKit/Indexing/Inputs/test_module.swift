public class Empty {}

public class TwoInts {
  var x, y : Int
  
  public init(a : Int, b : Int) {
    x = a
    y = b
  }
}

public class ComputedProperty {
  public var value : Int {
    get {
      var result = 0
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
