@_exported import my_system_overlay

public func some_func() {}

public class BaseCls {
  func theMeth() {}
}

public class SubCls : BaseCls {
  override func theMeth() {}
}
