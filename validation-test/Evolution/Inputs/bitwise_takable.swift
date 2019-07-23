public protocol Reporter  {
  func report() -> String
}
public class Subject {
  public var value = 1
  public init(_ v: Int) {
    value = v
  }
}
public var s2 = Subject(2)
public var s3 = Subject(3)
public var s4 = Subject(4)
public var s5 = Subject(5)

public struct Container : Reporter {
#if BEFORE
  var v : Subject
#else
  weak var v : Subject?
#endif
  public init(_ s: Subject) {
    v = s
  }

  public func report() -> String{
#if BEFORE
    return "Container(\(v.value))"
#else
    return "Container(\(v!.value))"
#endif
  }
}
public func createContainerReporter() -> Reporter {
  return Container(s2)
}

public struct PairContainer: Reporter {
  public var pair : (Container, Container)

  public init(_ p : (Container, Container)) {
    pair = p
  }

  public func report() -> String {
    return "PairContainer(\(pair.0.report()), \(pair.1.report()))"
  }
}

public func createPairContainerReporter() -> Reporter {
  return PairContainer((Container(s3), Container(s4)))
}

public enum EnumContainer : Reporter {
  case Empty
  case Some(Container)

  public func report() -> String {
    switch self {
      case .Empty:
         return "EnumContainer Empty"
      case .Some(let c):
        return "EnumContainer(\(c.report()))"
    }
  }
}

public func createEnumContainerReporter() -> Reporter {
  return EnumContainer.Some(Container(s5))
}

public func report(_ r: Reporter) -> String {
  return r.report()
}
