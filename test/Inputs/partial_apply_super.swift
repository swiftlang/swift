import OutsideClasses

public class Parent {
  public init() {}
  public func method() {}
  public class func classMethod() {}
}

public class GenericParent<A> {
  let a: A
  public init(a: A) {
    self.a = a
  }
  public func method() {}
  public class func classMethod() {}
}

class Child : Parent {
  override func method() {
    doFoo(super.method)
  }

  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GenericChild<A> : GenericParent<A> {
  override init(a: A) {
    super.init(a: a)
  }

  override func method() {
    doFoo(super.method)
  }

  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GrandchildToOutside : OutsideChild {
  override func method() {
    doFoo(super.method)
  }

  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

let c = {
  class Child : Parent {
    override func method() {
      doFoo(super.method)
    }

    override class func classMethod() {
      doFoo(super.classMethod)
    }
  }

  class GrandchildToOutside : OutsideChild {
    override func method() {
      doFoo(super.method)
    }

    override class func classMethod() {
      doFoo(super.classMethod)
    }
  }
}

class GenericGrandchildToOutside<A> : GenericOutsideChild<A> {
  override func method() {
    doFoo(super.method)
  }

  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

let cHasGenerics = {
  class GenericChild<A> : GenericParent<A> {
    override init(a: A) {
      super.init(a: a)
    }

    override func method() {
      doFoo(super.method)
    }

    override class func classMethod() {
      doFoo(super.classMethod)
    }
  }

  class GenericGrandchildToOutside<A> : GenericOutsideChild<A> {
    override func method() {
      doFoo(super.method)
    }

    override class func classMethod() {
      doFoo(super.classMethod)
    }
  }
}
