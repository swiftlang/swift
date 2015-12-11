public func doFoo(f: () -> ()) {
  f()
}

public class OutsideParent {
  public var property: String = "OutsideParent.property"

  public class var classProperty: String {
    return "OutsideParent.classProperty"
  }

  public init() {
    print("OutsideParent.init()")
  }

  public func method() {
    print("OutsideParent.method()")
  }

  public class func classMethod() {
    print("OutsideParent.classMethod()")
  }
}

public class OutsideChild : OutsideParent {
  public override func method() {
    print("OutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("OutsideChild.classMethod()")
    super.classMethod()
  }
}

public class GenericOutsideParent<A> {
  public var property: A
  public init(property: A) {
    self.property = property
    print("OutsideParent.init()")
  }

  public func method() {
    print("OutsideParent.method()")
  }

  public class func classMethod() {
    print("OutsideParent.classMethod()")
  }
}

public class GenericOutsideChild<A> : GenericOutsideParent<A> {
  public override init(property: A) {
    print("OutsideGenericChild.init(a: A)")
    super.init(property: property)
  }

  public override func method() {
    print("OutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("OutsideChild.classMethod()")
    super.classMethod()
  }
}

public class ConcreteOutsideChild : GenericOutsideParent<String> {
  public override init(property: String) {
    print("ConcreteOutsideChild.init(property: String)")
    super.init(property: property)
  }

  public override func method() {
    print("OutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("OutsideChild.classMethod()")
    super.classMethod()
  }
}
