
import SwiftShims
import _SwiftCOMShims

#if os(Windows)
public typealias HRESULT = CLong
#else
public typealias HRESULT = Int32
#endif

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

@implementation @c
@_alwaysEmitIntoClient
public func QueryInterface(_ pUnk: UnsafeMutableRawPointer,
                           _ riid: UnsafeRawPointer,
                           _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> HRESULT {
  fatalError("QueryInterface")
}

@implementation @c
@_alwaysEmitIntoClient
public func AddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  fatalError("AddRef")
}

@implementation @c
@_alwaysEmitIntoClient
public func Release(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  fatalError("Release")
}

#if $_MicrosoftCOM

@implementation @c
@_alwaysEmitIntoClient
public func AggregatedQueryInterface(_ pUnk: UnsafeMutableRawPointer,
                                     _ riid: UnsafeRawPointer,
                                     _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> HRESULT {
  fatalError("AggregatedQueryInterface")
}

@implementation @c
@_alwaysEmitIntoClient
public func AggregatedAddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  fatalError("AggregatedAddRef")
}

@implementation @c
@_alwaysEmitIntoClient
public func AggregatedRelease(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  fatalError("AggregatedRelease")
}

#endif
