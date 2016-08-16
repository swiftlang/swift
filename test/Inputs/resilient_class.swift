
import resilient_struct


// Fixed-layout, fixed-size base class

@_fixed_layout
open class OutsideParent {
  open var property: String = "OutsideParent.property"

  open class var classProperty: String {
    return "OutsideParent.classProperty"
  }

  public init() {
    print("OutsideParent.init()")
  }

  open func method() {
    print("OutsideParent.method()")
  }

  open class func classMethod() {
    print("OutsideParent.classMethod()")
  }
}


// Fixed-layout, resiliently-sized base class

@_fixed_layout
open class OutsideParentWithResilientProperty {
  public let p: Point
  public let s: Size
  public let color: Int32

  public init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}


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
}


// Fixed-layout, fixed-size subclass

@_fixed_layout
open class OutsideChild : OutsideParent {
  open override func method() {
    print("OutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("OutsideChild.classMethod()")
    super.classMethod()
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


// Fixed-layout, dependently-sized, generic base class

@_fixed_layout
open class GenericOutsideParent<A> {
  open var property: A
  public init(property: A) {
    self.property = property
    print("GenericOutsideParent.init()")
  }

  open func method() {
    print("GenericOutsideParent.method()")
  }

  open class func classMethod() {
    print("GenericOutsideParent.classMethod()")
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


// Fixed-layout, dependently-sized, generic subclass

@_fixed_layout
open class GenericOutsideChild<A> : GenericOutsideParent<A> {
  public override init(property: A) {
    print("GenericOutsideGenericChild.init(a: A)")
    super.init(property: property)
  }

  open override func method() {
    print("GenericOutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("GenericOutsideChild.classMethod()")
    super.classMethod()
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


// Fixed-layout, fixed-size subclass of generic class

@_fixed_layout
open class ConcreteOutsideChild : GenericOutsideParent<String> {
  public override init(property: String) {
    print("ConcreteOutsideChild.init(property: String)")
    super.init(property: property)
  }

  open override func method() {
    print("ConcreteOutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("ConcreteOutsideChild.classMethod()")
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
