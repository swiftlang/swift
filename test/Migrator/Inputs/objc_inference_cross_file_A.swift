import Foundation

public class MyClass : NSObject {
  public func methodUsedInSelector() {}
  public var propertyUsedInKeyPath: Int { return 2 }
  dynamic var dynamicVarUsedInSelector: Int { return 2 }
}
