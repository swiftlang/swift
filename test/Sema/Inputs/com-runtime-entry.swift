public struct GUID {
  public var data1: UInt32
  public var data2: UInt16
  public var data3: UInt16
  public var data4: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)

  public init(data1: UInt32, data2: UInt16, data3: UInt16,
              data4: (UInt8, UInt8, UInt8, UInt8,
                      UInt8, UInt8, UInt8, UInt8)) {
    self.data1 = data1
    self.data2 = data2
    self.data3 = data3
    self.data4 = data4
  }
}
public typealias IID = GUID

@com(interface: "8e369447-5188-5ada-b9ec-8fcb732d226b")
public protocol ISwiftObject {
  var object: UnsafeMutableRawPointer { get }
  var metadata: UnsafeRawPointer { get }
}

public protocol COMInterface {
  var IID: IID { get }
}

#if $_MicrosoftCOM
@com(interface: "00000000-0000-0000-C000-000000000046")
public protocol IUnknown: AnyObject { }

public protocol COMAggregatable: AnyObject {
  var controller: (any IUnknown)? { get }
}
#endif

#if INVALID_QUERY_INTERFACE
public func QueryInterface(_ pUnk: UnsafeMutableRawPointer,
                           _ riid: UnsafeRawPointer,
                           _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> CInt {
  0
}
#elseif !MISSING_QUERY_INTERFACE
@c
public func QueryInterface(_ pUnk: UnsafeMutableRawPointer,
                           _ riid: UnsafeRawPointer,
                           _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> CInt {
  0
}
#endif

#if AMBIGUOUS_QUERY_INTERFACE
@c(QueryInterfaceOverload)
public func QueryInterface(_ value: UInt32) -> UInt32 {
  value
}
#endif

// A Swift overload does not make the direct C entry ambiguous.
public func QueryInterface(_ value: Int) -> Int {
  value
}

@c
public func AddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  0
}

#if !MISSING_RELEASE
@c
public func Release(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  0
}
#endif
