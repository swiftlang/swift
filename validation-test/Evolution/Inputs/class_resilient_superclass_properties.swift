public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

open class Base {
  public init() {}
  open var property: String {
    return "Base.property"
  }
  open var nonOverriddenProperty: String {
    return "Base.nonOverriddenProperty"
  }
  open class var classProperty: String {
    return "Base.classProperty"
  }
}

open class InBetween : Base {
  open override var property: String {
    return "InBetween.property"
  }
  open override class var classProperty: String {
    return "InBetween.classProperty"
  }
}

#if BEFORE
open class AddInterposingProperty : Base {}
#else
open class AddInterposingProperty : Base {
  open override var property: String {
    return "AddInterposingProperty.property"
  }
  open override class var classProperty: String {
    return "AddInterposingProperty.classProperty"
  }
}
#endif

#if BEFORE
open class RemoveInterposingProperty : Base {
  open override var property: String {
    return "RemoveInterposingProperty.property"
  }
  open override class var classProperty: String {
    return "RemoveInterposingProperty.classProperty"
  }
}
#else
open class RemoveInterposingProperty : Base {}
#endif

#if BEFORE
open class InsertSuperclass : Base {}
#else
open class InsertSuperclass : InBetween {}
#endif
