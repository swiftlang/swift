public func doFoo(f: () -> ()) {
  f()
}

@_fixed_layout
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

public class ResilientOutsideParent {
  public var property: String = "ResilientOutsideParent.property"
  public final var finalProperty: String = "ResilientOutsideParent.finalProperty"

  public class var classProperty: String {
    return "ResilientOutsideParent.classProperty"
  }

  public init() {
    print("ResilientOutsideParent.init()")
  }

  public func method() {
    print("ResilientOutsideParent.method()")
  }

  public class func classMethod() {
    print("ResilientOutsideParent.classMethod()")
  }
}

@_fixed_layout
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

public class ResilientOutsideChild : ResilientOutsideParent {
  public override func method() {
    print("ResilientOutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("ResilientOutsideChild.classMethod()")
    super.classMethod()
  }
}

@_fixed_layout
public class GenericOutsideParent<A> {
  public var property: A
  public init(property: A) {
    self.property = property
    print("GenericOutsideParent.init()")
  }

  public func method() {
    print("GenericOutsideParent.method()")
  }

  public class func classMethod() {
    print("GenericOutsideParent.classMethod()")
  }
}

public class ResilientGenericOutsideParent<A> {
  public var property: A
  public init(property: A) {
    self.property = property
    print("ResilientGenericOutsideParent.init()")
  }

  public func method() {
    print("ResilientGenericOutsideParent.method()")
  }

  public class func classMethod() {
    print("ResilientGenericOutsideParent.classMethod()")
  }
}

@_fixed_layout
public class GenericOutsideChild<A> : GenericOutsideParent<A> {
  public override init(property: A) {
    print("GenericOutsideGenericChild.init(a: A)")
    super.init(property: property)
  }

  public override func method() {
    print("GenericOutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("GenericOutsideChild.classMethod()")
    super.classMethod()
  }
}

public class ResilientGenericOutsideChild<A> : ResilientGenericOutsideParent<A> {
  public override init(property: A) {
    print("ResilientGenericOutsideGenericChild.init(a: A)")
    super.init(property: property)
  }

  public override func method() {
    print("ResilientGenericOutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("ResilientGenericOutsideChild.classMethod()")
    super.classMethod()
  }
}

@_fixed_layout
public class ConcreteOutsideChild : GenericOutsideParent<String> {
  public override init(property: String) {
    print("ConcreteOutsideChild.init(property: String)")
    super.init(property: property)
  }

  public override func method() {
    print("ConcreteOutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("ConcreteOutsideChild.classMethod()")
    super.classMethod()
  }
}

public class ResilientConcreteOutsideChild : ResilientGenericOutsideParent<String> {
  public override init(property: String) {
    print("ResilientConcreteOutsideChild.init(property: String)")
    super.init(property: property)
  }

  public override func method() {
    print("ResilientConcreteOutsideChild.method()")
    super.method()
  }

  public override class func classMethod() {
    print("ResilientConcreteOutsideChild.classMethod()")
    super.classMethod()
  }
}
