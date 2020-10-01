@_exported import my_system_overlay

public func some_func() {}

public class BaseCls {
  public func theMeth() {}
  internal func SECRET() {}
}

public class SubCls : BaseCls {
  public override func theMeth() {}
}
