public func doFoo(f: () -> ()) {
  f()
}

public class OutsideParent {
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
  let a: A
  public init(a: A) {
    self.a = a
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
  public override init(a: A) {
    print("OutsideGenericChild.init(a: A)")
    super.init(a: a)
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
  public override init(a: String) {
    print("ConcreteOutsideChild.init(s: String)")
    super.init(a: a)
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
