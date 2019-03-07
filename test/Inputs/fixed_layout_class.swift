
import resilient_struct


// Fixed-layout, fixed-size base class

@_fixed_layout
open class OutsideParent {
  public final var property: String = "OutsideParent.property"

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

  public final lazy var laziestNumber = 0

  public init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}


// Fixed-layout, fixed-size subclass

@_fixed_layout
open class OutsideChild : OutsideParent {
  public let childProperty: Int = 0

  open override func method() {
    print("OutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("OutsideChild.classMethod()")
    super.classMethod()
  }
}


// Fixed-layout, dependently-sized, generic base class

@_fixed_layout
open class GenericOutsideParent<A> {
  public final var property: A
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


// Fixed-layout, dependently-sized, generic subclass

@_fixed_layout
open class GenericOutsideChild<A> : GenericOutsideParent<A> {
  public final var childProperty: A

  public override init(property: A) {
    self.childProperty = property
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


// Fixed-layout, fixed-size subclass of generic class

@_fixed_layout
open class ConcreteOutsideChild : GenericOutsideParent<String> {
  public final var childProperty: Int = 0

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
