public class MyClass {
  public func pub_method() {}
  internal func int_method() {}
  fileprivate func fp_method() {}
  private func priv_method() {}
}

public func pub_function() {}
internal func int_function() {}
fileprivate func fp_function() {}
private func priv_function() {}

public struct MyStruct {
  public mutating func mutatingFunc() {}
  public var mutVar: Int {
    mutating get { 1 }
    nonmutating set {}
  }
}
