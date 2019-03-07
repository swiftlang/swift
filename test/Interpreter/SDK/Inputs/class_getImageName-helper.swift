import Foundation

public class SimpleSwiftObject {}
public class SimpleNSObject: NSObject {
  @objc public dynamic var observableName: String = ""
}

public class GenericSwiftObject<T> {}
public class GenericNSObject<T>: NSObject {}

public class GenericAncestrySwiftObject: GenericSwiftObject<AnyObject> {}
public class GenericAncestryNSObject: GenericNSObject<AnyObject> {
  @objc public dynamic var observableName: String = ""
}

public class ResilientFieldSwiftObject {
  public var url: URL?
  public var data: Data?
}
public class ResilientFieldNSObject: NSObject {
  public var url: URL?
  public var data: Data?
}
