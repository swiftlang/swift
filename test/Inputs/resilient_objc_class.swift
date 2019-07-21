
import resilient_struct
import Foundation


// Resilient base class

open class ResilientNSObjectOutsideParent: NSObject {
  open var property: String = "ResilientNSObjectOutsideParent.property"
  public final var finalProperty: String = "ResilientNSObjectOutsideParent.finalProperty"

  open class var classProperty: String {
    return "ResilientNSObjectOutsideParent.classProperty"
  }

  override public init() {
    print("ResilientNSObjectOutsideParent.init()")
  }

  open func method() {
    print("ResilientNSObjectOutsideParent.method()")
  }

  open class func classMethod() {
    print("ResilientNSObjectOutsideParent.classMethod()")
  }

  open func getValue() -> Int {
    return 0
  }
}

// Resilient generic base class

open class ResilientGenericNSObjectOutsideParent<A>: NSObject {
  open var property: A
  public init(property: A) {
    self.property = property
    print("ResilientGenericNSObjectOutsideParent.init()")
  }

  open func method() {
    print("ResilientGenericNSObjectOutsideParent.method()")
  }

  open class func classMethod() {
    print("ResilientGenericNSObjectOutsideParent.classMethod()")
  }
}

// Resilient subclass of generic class

open class ResilientConcreteNSObjectOutsideChild : ResilientGenericNSObjectOutsideParent<String> {
  public override init(property: String) {
    print("ResilientConcreteNSObjectOutsideChild.init(property: String)")
    super.init(property: property)
  }

  open override func method() {
    print("ResilientConcreteNSObjectOutsideChild.method()")
    super.method()
  }

  open override class func classMethod() {
    print("ResilientConcreteNSObjectOutsideChild.classMethod()")
    super.classMethod()
  }
}

// Fixed-layout base class

@_fixed_layout
open class FixedLayoutNSObjectOutsideParent: NSObject {}