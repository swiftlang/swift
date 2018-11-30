
import resilient_struct



// Resilient base class

open class ResilientOutsideParent {
  open var property: String = "ResilientOutsideParent.property"
  public final var finalProperty: String = "ResilientOutsideParent.finalProperty"

  open class var classProperty: String {
    return "ResilientOutsideParent.classProperty"
  }

  public init() {
    print("ResilientOutsideParent.init()")
  }

  open func method() {
    print("ResilientOutsideParent.method()")
  }

  open class func classMethod() {
    print("ResilientOutsideParent.classMethod()")
  }

  open func getValue() -> Int {
    return 0
  }
}



// Resilient subclass

open class ResilientOutsideChild : ResilientOutsideParent {
  open override func method() {
    print("ResilientOutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("ResilientOutsideChild.classMethod()")
    super.classMethod()
  }
}


// Resilient generic base class

open class ResilientGenericOutsideParent<A> {
  open var property: A
  public init(property: A) {
    self.property = property
    print("ResilientGenericOutsideParent.init()")
  }

  open func method() {
    print("ResilientGenericOutsideParent.method()")
  }

  open class func classMethod() {
    print("ResilientGenericOutsideParent.classMethod()")
  }
}


// Resilient generic subclass

open class ResilientGenericOutsideChild<A> : ResilientGenericOutsideParent<A> {
  public override init(property: A) {
    print("ResilientGenericOutsideGenericChild.init(a: A)")
    super.init(property: property)
  }

  open override func method() {
    print("ResilientGenericOutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("ResilientGenericOutsideChild.classMethod()")
    super.classMethod()
  }
}


// Resilient subclass of generic class

open class ResilientConcreteOutsideChild : ResilientGenericOutsideParent<String> {
  public override init(property: String) {
    print("ResilientConcreteOutsideChild.init(property: String)")
    super.init(property: property)
  }

  open override func method() {
    print("ResilientConcreteOutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("ResilientConcreteOutsideChild.classMethod()")
    super.classMethod()
  }
}
