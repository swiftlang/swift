// Do not add an import of the Clang "Mixed" module here!

@objc public class SwiftClass {
  public init(x: Int) {}
  public func pureSwiftMethod(_ x: Int?) -> Bool {
    return x != nil ? true : false
  }

  @objc public func method() {}
  @objc public var integerProperty: Int = 0
}

extension SwiftClass {
  @objc public func extensionMethod() {}
}

public class PureSwiftClass {
  public class func verify() -> Bool { return true }
}

@objc(SwiftProtoWithCustomName)
public protocol CustomName {}

@objc(SwiftClassWithCustomName)
public class CustomNameClass : CustomName {
  public init() {}
  @nonobjc func pureSwiftMethod() {}
}

@objc public protocol SwiftProto {
  @objc func protoMethod()
  @objc var protoProperty: Int { get }
}

@objc
public class ObjCClass {
  public init() {}
}

public class ImplicitlyObjCClass : ObjCClass {
    public override init() { super.init() }
}

@objc
public class ExplicitlyObjCClass {
    public init() {}
}

@objc(HasSameCustomNameClass)
public class HasSameCustomNameClass {
    public init() {}
}

@objc(ObjCNativeTypeHasDifferentCustomNameClass)
public class NativeTypeHasDifferentCustomNameClass {
    public init() {}
}
@objc(NativeTypeHasDifferentCustomNameClass)
public class SwiftNativeTypeHasDifferentCustomNameClass {
    public init() {}
}

public class NativeTypeIsNonObjCClass {
    public init() {}
}
@objc(NativeTypeIsNonObjCClass)
public class SwiftNativeTypeIsNonObjCClass {
    public init() {}
}

public class ForwardImplicitlyObjCClass : ObjCClass {
    public override init() { super.init() }
}

@objc
public class ForwardExplicitlyObjCClass {
    public init() {}
}

@objc(ForwardHasSameCustomNameClass)
public class ForwardHasSameCustomNameClass {
    public init() {}
}

@objc(ObjCForwardNativeTypeHasDifferentCustomNameClass)
public class ForwardNativeTypeHasDifferentCustomNameClass {
    public init() {}
}
@objc(ForwardNativeTypeHasDifferentCustomNameClass)
public class SwiftForwardNativeTypeHasDifferentCustomNameClass {
    public init() {}
}

public class ForwardNativeTypeIsNonObjCClass {
    public init() {}
}
@objc(ForwardNativeTypeIsNonObjCClass)
public class SwiftForwardNativeTypeIsNonObjCClass {
    public init() {}
}

public class ForwardNativeTypeIsUnambiguouslyNonObjCClass {
    public init() {}
}
