public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public class Base {
  public init() {}
  public var property: String {
    return "Base.property"
  }
  public class var classProperty: String {
    return "Base.classProperty"
  }
}

public class OtherBase {
  public init() {}
  public var property: String {
    return "OtherBase.property"
  }
  public class var classProperty: String {
    return "OtherBase.classProperty"
  }
}

public class InBetween : Base {
  public override var property: String {
    return "InBetween.property"
  }
  public override class var classProperty: String {
    return "InBetween.classProperty"
  }
}

#if BEFORE
public class AddInterposingProperty : Base {}
#else
public class AddInterposingProperty : Base {
  public override var property: String {
    return "AddInterposingProperty.property"
  }
  public override class var classProperty: String {
    return "AddInterposingProperty.classProperty"
  }
}
#endif

#if BEFORE
public class RemoveInterposingProperty : Base {
  public override var property: String {
    return "RemoveInterposingProperty.property"
  }
  public override class var classProperty: String {
    return "RemoveInterposingProperty.classProperty"
  }
}
#else
public class RemoveInterposingProperty : Base {}
#endif

#if BEFORE
public class InsertSuperclass : Base {}
#else
public class InsertSuperclass : InBetween {}
#endif

#if BEFORE
public class ChangeRoot : Base {}
#else
public class ChangeRoot : OtherBase {}
#endif
