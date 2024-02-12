
import resilient_struct



// Resilient base class

package class ResilientOutsideParent {
  package var property: String = "ResilientOutsideParent.property"
  package final var finalProperty: String = "ResilientOutsideParent.finalProperty"

  package class var classProperty: String {
    return "ResilientOutsideParent.classProperty"
  }

  package init() {
    print("ResilientOutsideParent.init()")
  }

  package func method() {
    print("ResilientOutsideParent.method()")
  }

  package class func classMethod() {
    print("ResilientOutsideParent.classMethod()")
  }

  package func getValue() -> Int {
    return 0
  }
}



// Resilient subclass

package class ResilientOutsideChild : ResilientOutsideParent {
  package override func method() {
    print("ResilientOutsideChild.method()")
    super.method()
  }

  package override class func classMethod() {
    print("ResilientOutsideChild.classMethod()")
    super.classMethod()
  }
}


// Resilient generic base class

package class ResilientGenericOutsideParent<A> {
  package var property: A
  package init(property: A) {
    self.property = property
    print("ResilientGenericOutsideParent.init()")
  }

  package func method() {
    print("ResilientGenericOutsideParent.method()")
  }

  package class func classMethod() {
    print("ResilientGenericOutsideParent.classMethod()")
  }
}

public class PublicGenericOutsideParent<A> {
  public var property: A
  public init(property: A) {
    self.property = property
    print("PublicGenericOutsideParent.init()")
  }

  public func method() {
    print("PublicGenericOutsideParent.method()")
  }

  public class func classMethod() {
    print("PublicGenericOutsideParent.classMethod()")
  }
}


// Resilient generic subclass

package class ResilientGenericOutsideChild<A> : ResilientGenericOutsideParent<A> {
  package override init(property: A) {
    print("ResilientGenericOutsideGenericChild.init(a: A)")
    super.init(property: property)
  }

  package override func method() {
    print("ResilientGenericOutsideChild.method()")
    super.method()
  }

  package override class func classMethod() {
    print("ResilientGenericOutsideChild.classMethod()")
    super.classMethod()
  }
}


// Resilient subclass of generic class

package class ResilientConcreteOutsideChild : ResilientGenericOutsideParent<String> {
  package override init(property: String) {
    print("ResilientConcreteOutsideChild.init(property: String)")
    super.init(property: property)
  }

  package override func method() {
    print("ResilientConcreteOutsideChild.method()")
    super.method()
  }

  package override class func classMethod() {
    print("ResilientConcreteOutsideChild.classMethod()")
    super.classMethod()
  }
}
