public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

open class Base {
  public init() {}
  open func method() -> String {
    return "Base.method()"
  }
  open class func classMethod() -> String {
    return "Base.classMethod()"
  }

  open func nonOverriddenMethod() -> String {
    return "Base.nonOverriddenMethod()"
  }
}

open class InBetween : Base {
  open override func method() -> String {
    return "InBetween.method()"
  }
  open override class func classMethod() -> String {
    return "InBetween.classMethod()"
  }
}

#if BEFORE
open class AddInterposingMethod : Base {}
#else
open class AddInterposingMethod : Base {
  open override func method() -> String {
    return "AddInterposingMethod.method()"
  }
  open override class func classMethod() -> String {
    return "AddInterposingMethod.classMethod()"
  }
}
#endif

#if BEFORE
open class RemoveInterposingMethod : Base {
  open override func method() -> String {
    return "RemoveInterposingMethod.method()"
  }
  open override class func classMethod() -> String {
    return "RemoveInterposingMethod.classMethod()"
  }
}
#else
open class RemoveInterposingMethod : Base {}
#endif

#if BEFORE
open class InsertSuperclass : Base {}
#else
open class InsertSuperclass : InBetween {}
#endif
