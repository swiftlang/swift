public protocol Base {}
public protocol Derived : Base {}

public protocol HasAssocType {
  associatedtype T : Derived

  var value: T { get }
}

public class ConcreteDerived : Derived {}

public class ConcreteHasAssocType : HasAssocType {
  public var value: ConcreteDerived { fatalError() }
}
