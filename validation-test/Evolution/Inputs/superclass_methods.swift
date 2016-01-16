public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public class Base {
  public init() {}
  public func method() -> String {
    return "Base.method()"
  }
  public class func classMethod() -> String {
    return "Base.classMethod()"
  }
}

public class OtherBase {
  public init() {}
  public func method() -> String {
    return "OtherBase.method()"
  }
  public class func classMethod() -> String {
    return "OtherBase.classMethod()"
  }
}

public class InBetween : Base {
  public override func method() -> String {
    return "InBetween.method()"
  }
  public override class func classMethod() -> String {
    return "InBetween.classMethod()"
  }
}

#if BEFORE
public class AddInterposingMethod : Base {}
#else
public class AddInterposingMethod : Base {
  public override func method() -> String {
    return "AddInterposingMethod.method()"
  }
  public override class func classMethod() -> String {
    return "AddInterposingMethod.classMethod()"
  }
}
#endif

#if BEFORE
public class RemoveInterposingMethod : Base {
  public override func method() -> String {
    return "RemoveInterposingMethod.method()"
  }
  public override class func classMethod() -> String {
    return "RemoveInterposingMethod.classMethod()"
  }
}
#else
public class RemoveInterposingMethod : Base {}
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
