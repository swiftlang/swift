
import SwiftShims
import _SwiftCOMShims

#if os(Windows)
public typealias HRESULT = CLong
#else
public typealias HRESULT = Int32
#endif

public var S_OK: HRESULT {
  0
}

public var E_NOINTERFACE: HRESULT {
  HRESULT(truncatingIfNeeded: 0x8000_4002 as UInt32)
}

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

extension GUID: Equatable {
  public static func == (_ lhs: GUID, _ rhs: GUID) -> Bool {
    withUnsafeBytes(of: lhs) { lhs in
      withUnsafeBytes(of: rhs) { rhs in
        lhs.elementsEqual(rhs)
      }
    }
  }
}

public typealias IID = GUID
public typealias CLSID = GUID

@com(interface: "00000000-0000-0000-C000-000000000046")
public protocol IUnknown: AnyObject { }

#if $_MicrosoftCOM

extension ObjectIdentifier {
  @_alwaysEmitIntoClient
  public init<Interface>(_ interface: borrowing Interface)
      where Interface.Type: COMInterface {
    let pUnk = unsafeBitCast(interface, to: UnsafeMutableRawPointer.self)
    let vtable = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
    let queryInterface =
      unsafeBitCast(vtable[0], to: _SwiftCOMQueryInterfaceFunction.self)

    var identity: UnsafeMutableRawPointer?
    let hr = withUnsafePointer(to: IUnknown.IID) { iid in
      queryInterface(pUnk, UnsafeRawPointer(iid), &identity)
    }
    guard hr >= 0, let identity else {
      preconditionFailure("QueryInterface for IUnknown failed")
    }

    self = unsafeBitCast(identity, to: ObjectIdentifier.self)

    let identityVTable =
      identity.load(as: UnsafePointer<UnsafeRawPointer>.self)
    let release =
      unsafeBitCast(identityVTable[2], to: _SwiftCOMLifetimeFunction.self)
    _ = release(identity)
  }
}

@_alwaysEmitIntoClient
public func === <Left, Right>(_ lhs: borrowing Left,
                              _ rhs: borrowing Right) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  return ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
}

@_alwaysEmitIntoClient
public func === <Left, Right>(_ lhs: Left?, _ rhs: Right?) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  guard let lhs else {
    return rhs == nil
  }
  guard let rhs else {
    return false
  }
  return lhs === rhs
}

@_alwaysEmitIntoClient
public func !== <Left, Right>(_ lhs: borrowing Left,
                              _ rhs: borrowing Right) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  return !(lhs === rhs)
}

@_alwaysEmitIntoClient
public func !== <Left, Right>(_ lhs: Left?, _ rhs: Right?) -> Bool
    where Left.Type: COMInterface, Right.Type: COMInterface {
  return !(lhs === rhs)
}

#endif

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

@implementation @c
@_alwaysEmitIntoClient
public func QueryInterface(_ pUnk: UnsafeMutableRawPointer,
                           _ riid: UnsafeRawPointer,
                           _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> HRESULT {
  ppvObject.pointee = nil

  let riid = riid.load(as: IID.self)
  let lpVtbl = pUnk.load(as: UnsafePointer<UnsafeRawPointer>.self)
  let object = pUnk.advanced(by: Int(bitPattern: lpVtbl[-1]))
  let map = lpVtbl[-2]
  let header = map.load(as: _SwiftCOMInterfaceMapHeader.self)
  let entries = map.advanced(by: MemoryLayout<_SwiftCOMInterfaceMapHeader>.stride)

  for index in 0 ..< Int(header.count) {
    let address = entries.advanced(by: index * MemoryLayout<_SwiftCOMInterfaceMapEntry>.stride)
    let entry = address.load(as: _SwiftCOMInterfaceMapEntry.self)

    var descriptor = address.advanced(by: Int(entry.descriptor & -2))
    if entry.descriptor & 1 == 1 {
      descriptor = descriptor.load(as: UnsafeRawPointer.self)
    }

    let iid = descriptor.advanced(by: MemoryLayout<_SwiftProtocolDescriptorHeader>.stride)
    guard iid.load(as: IID.self) == riid else {
      continue
    }

    let projection = object.advanced(by: -(Int(entry.index) + 1) * MemoryLayout<UnsafeRawPointer>.stride)
    ppvObject.pointee = projection
    _ = AddRef(projection)
    return S_OK
  }

  return E_NOINTERFACE
}

@implementation @c
@_alwaysEmitIntoClient
public func AddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  _ = Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).retain()
  return 1
}

@implementation @c
@_alwaysEmitIntoClient
public func Release(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  Unmanaged<AnyObject>.from(unsafeCOMPointer: pUnk).release()
  return 1
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
