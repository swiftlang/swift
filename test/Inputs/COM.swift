
public struct GUID {
  public var data1: UInt32
  public var data2: UInt16
  public var data3: UInt16
  public var data4: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)

  public init(data1: UInt32, data2: UInt16, data3: UInt16,
              data4: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)) {
    self.data1 = data1
    self.data2 = data2
    self.data3 = data3
    self.data4 = data4
  }
}

public typealias IID = GUID
public typealias CLSID = GUID

@com(interface: "00000000-0000-0000-C000-000000000046")
public protocol IUnknown: AnyObject { }

@com(interface: "8e369447-5188-5ada-b9ec-8fcb732d226b")
public protocol ISwiftObject {
  var object: UnsafeMutableRawPointer { get }
  var metadata: UnsafeRawPointer { get }
}

extension ISwiftObject {
  @_alwaysEmitIntoClient
  public var object: UnsafeMutableRawPointer {
    unsafeBitCast(self, to: UnsafeMutableRawPointer.self)
  }

  @_alwaysEmitIntoClient
  public var metadata: UnsafeRawPointer {
    unsafeBitCast(type(of: self), to: UnsafeRawPointer.self)
  }
}

public protocol COMInterface {
  var IID: IID { get }
}

#if $_MicrosoftCOM

public protocol COMActivatable {
  var CLSID: CLSID { get }
}

public protocol COMAggregatable: AnyObject {
  var controller: (any IUnknown)? { get }
}

#endif

extension Unmanaged where Instance == AnyObject {
  @usableFromInline
  @inline(__always)
  internal static func from(unsafeCOMPointer pUnk: UnsafeMutableRawPointer) -> Self {
    let lpVtbl = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
    return .fromOpaque(pUnk.advanced(by: Int(bitPattern: lpVtbl[-1])))
  }
}

@c
@_alwaysEmitIntoClient
public func QueryInterface(_ pUnk: UnsafeMutableRawPointer,
                           _ riid: UnsafeRawPointer,
                           _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> CInt {
  fatalError("QueryInterface")
}

@c
@_alwaysEmitIntoClient
public func AddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  _ = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).retain()
  return 1
}

@c
@_alwaysEmitIntoClient
public func Release(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).release()
  return 1
}

#if $_MicrosoftCOM

@c
@_alwaysEmitIntoClient
public func AggregatedQueryInterface(_ pUnk: UnsafeMutableRawPointer,
                                     _ riid: UnsafeRawPointer,
                                     _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> CInt {
  fatalError("AggregatedQueryInterface")
}

@c
@_alwaysEmitIntoClient
public func AggregatedAddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  fatalError("AggregatedAddRef")
}

@c
@_alwaysEmitIntoClient
public func AggregatedRelease(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  fatalError("AggregatedRelease")
}

#endif
