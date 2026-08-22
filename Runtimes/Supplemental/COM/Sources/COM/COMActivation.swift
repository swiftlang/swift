//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if $_MicrosoftCOM

public import WinSDK

/// Every proper activation semantic maps to exactly one public entry point:
///
///              Semantic             |               Family               |                 Return                 |
/// ----------------------------------+------------------------------------+----------------------------------------+
/// one interface, local or remote    | CoCreateInstance(…, as:)           | Interface                              |
/// N ≥ 1 interfaces, local or remote | CoCreateInstanceEx(…, requesting:) | (Interface, repeat (each Interfaces)?) |
/// aggregation (IUnknown, in-proc)   | CoCreateInstance(…, aggregating:)  | any IUnknown                           |
///
/// IClassFactory::CreateInstance mandates that `riid` is `IID_IUnknown` when
/// the interface is aggregated.


@usableFromInline
@_alwaysEmitIntoClient
package func CoCreateInstance<Interface>(_ clsid: borrowing CLSID,
                                         activation context: CLSCTX = COMActivationOptions.current.context,
                                         server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                         outer: borrowing IUnknown? = nil,
                                         as: Interface.Type = Interface.self)
    throws(COMError) -> Interface where Interface.Type: COMInterface {
  precondition(server == nil || outer == nil, "remote aggregation is disallowed")

  if let server {
    return try CoCreateInstanceEx(clsid, activation: context, server: server,
                                  as: Interface.self)
  }

  var instance: UnsafeMutableRawPointer?
  let hr = withUnsafePointer(to: clsid) {
    let rclsid = UnsafeRawPointer($0).assumingMemoryBound(to: WinSDK.CLSID.self)
    return withUnsafePointer(to: Interface.IID) {
      let riid = UnsafeRawPointer($0).assumingMemoryBound(to: WinSDK.IID.self)

      let pUnknown = outer.map {
        let pointer = ManagedObject<IUnknown>.passUnretained($0)
        return pointer.assumingMemoryBound(to: WinSDK.IUnknown.self)
      }
      return WinSDK.CoCreateInstance(rclsid, pUnknown, DWORD(context.rawValue),
                                     riid, &instance)
    }
  }

  guard SUCCEEDED(hr) else {
    throw COMError(hr: hr)
  }
  guard let instance else {
    throw COMError(hr: E_UNEXPECTED)
  }
  return ManagedObject<Interface>.takeRetainedValue(instance)
}

// TODO(compnerd): convert to throwing; determine if we can return the results without penalty
@_alwaysEmitIntoClient
package func CoCreateInstanceEx(_ clsid: CLSID,
                                activation context: CLSCTX,
                                server: COMServerInfo? = nil,
                                interfaces: Span<IID>,
                                results: UnsafeMutableBufferPointer<WinSDK.MULTI_QI>)
    -> HRESULT {
  if interfaces.isEmpty {
    return E_INVALIDARG
  }
  precondition(interfaces.count == results.count)

  return interfaces.withUnsafeBufferPointer { interfaces in
    return withUnsafePointer(to: clsid) {
      let rclsid =
          UnsafeRawPointer($0).assumingMemoryBound(to: WinSDK.CLSID.self)
      for index in results.indices {
        results[index].pIID =
            UnsafeRawPointer(interfaces.baseAddress!.advanced(by: index))
                .assumingMemoryBound(to: WinSDK.IID.self)
      }

      guard let server else {
        return CoCreateInstanceEx(rclsid, nil, DWORD(context.rawValue), nil,
                                  DWORD(results.count), results.baseAddress!)
      }

      return server.name.withCString(encodedAs: UTF16.self) { lpszServerName in
        var info = COSERVERINFO()
        info.pwszName = UnsafeMutablePointer(mutating: lpszServerName)
        return CoCreateInstanceEx(rclsid, nil, DWORD(context.rawValue), &info,
                                  DWORD(results.count), results.baseAddress!)
      }
    }
  }
}

@_alwaysEmitIntoClient
package func CoCreateInstanceEx<Interface>(_ clsid: CLSID,
                                           activation context: CLSCTX,
                                           server: consuming COMServerInfo? = nil,
                                           as interface: Interface.Type = Interface.self)
    throws(COMError) -> Interface where Interface.Type: COMInterface {
  let iid = Interface.IID
  var result = WinSDK.MULTI_QI()
  let hr = withUnsafePointer(to: iid) { iid in
    return withUnsafeMutablePointer(to: &result) { result in
      let buffer = UnsafeBufferPointer(start: iid, count: 1)
      return CoCreateInstanceEx(clsid, activation: context, server: server,
                                interfaces: buffer.span,
                                results: UnsafeMutableBufferPointer(start: result, count: 1))
    }
  }

  guard SUCCEEDED(hr) || hr == E_NOINTERFACE else {
    throw COMError(hr: hr)
  }
  guard SUCCEEDED(result.hr) else {
    throw COMError(hr: result.hr)
  }
  guard let pointer = result.pItf else {
    throw COMError(hr: E_UNEXPECTED)
  }
  let pointer = UnsafeMutableRawPointer(pointer)
  return ManagedObject<Interface>.takeRetainedValue(pointer)
}

// MARK: - CoCreateInstance

@_alwaysEmitIntoClient
public func CoCreateInstance<Interface>(_ clsid: borrowing CLSID,
                                        activation context: CLSCTX = COMActivationOptions.current.context,
                                        server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                        as: Interface.Type = Interface.self)
    throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try CoCreateInstance(clsid, activation: context, server: server,
                              outer: nil, as: Interface.self)
}

@_transparent
@_alwaysEmitIntoClient
public func CoCreateInstance<Interface>(_ clsid: borrowing WinSDK.GUID,
                                        activation context: CLSCTX = COMActivationOptions.current.context,
                                        server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                        as: Interface.Type = Interface.self)
    throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try CoCreateInstance(CLSID(clsid), activation: context, server: server,
                              as: Interface.self)
}

@_alwaysEmitIntoClient
public func CoCreateInstance<Implementation, Interface>(_ implementation: Implementation.Type,
                                                        activation context: CLSCTX = COMActivationOptions.current.context,
                                                        server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                        as: Interface.Type = Interface.self)
    throws(COMError) -> Interface
    where Implementation.Type: COMActivatable, Interface.Type: COMInterface {
  return try CoCreateInstance(Implementation.CLSID, activation: context,
                              server: server, as: Interface.self)
}

@_alwaysEmitIntoClient
public func CoCreateInstance(_ clsid: borrowing CLSID,
                             activation context: CLSCTX = COMActivationOptions.current.context,
                             aggregating outer: any IUnknown)
    throws(COMError) -> any IUnknown {
  return try CoCreateInstance(clsid, activation: context, server: nil,
                              outer: outer, as: IUnknown.self)
}

@_transparent @_alwaysEmitIntoClient
public func CoCreateInstance(_ clsid: borrowing WinSDK.GUID,
                             activation context: CLSCTX = COMActivationOptions.current.context,
                             aggregating outer: any IUnknown)
    throws(COMError) -> any IUnknown {
  return try CoCreateInstance(CLSID(clsid), activation: context,
                              aggregating: outer)
}

// MARK: - CoCreateInstanceEx

@_alwaysEmitIntoClient
package func CoCreateInstanceEx<PrimaryInterface, each SecondaryInterface>(_ clsid: CLSID,
                                                                           activation context: CLSCTX = COMActivationOptions.current.context,
                                                                           server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                                           requesting primary: PrimaryInterface.Type,
                                                                           _ interfaces: repeat (each SecondaryInterface).Type)
    throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  var count = 1
  for _ in repeat each interfaces {
    count += 1
  }

  var failure: HRESULT?
  let instances = withUnsafeTemporaryAllocation(of: IID.self, capacity: count) { storage in
    var iids = OutputSpan(buffer: storage, initializedCount: 0)
    iids.append(PrimaryInterface.IID)
    for interface in repeat each interfaces {
      iids.append(interface.IID)
    }

    return withUnsafeTemporaryAllocation(of: WinSDK.MULTI_QI.self,
                                         capacity: count) { results in
      results.initialize(repeating: WinSDK.MULTI_QI())
      defer { results.deinitialize() }

      let hr = CoCreateInstanceEx(clsid, activation: context, server: server,
                                  interfaces: iids.span, results: results)
      if SUCCEEDED(hr) || hr == E_NOINTERFACE {
        for index in results.indices where FAILED(results[index].hr) {
          results[index].pItf = nil
        }
      } else {
        failure = hr
      }

      var iterator = results[...]
      let primary = ManagedObject<PrimaryInterface>.takeRetainedValue(iterator.removeFirst().pItf)
      return (primary, repeat ManagedObject<each SecondaryInterface>.takeRetainedValue(iterator.removeFirst().pItf))
    }
  }

  if let failure {
    throw COMError(hr: failure)
  }
  return instances
}

@_transparent
@_alwaysEmitIntoClient
public func CoCreateInstanceEx<PrimaryInterface, each SecondaryInterface>(_ clsid: borrowing WinSDK.GUID,
                                                                          activation context: CLSCTX = COMActivationOptions.current.context,
                                                                          server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                                          requesting primary: PrimaryInterface.Type,
                                                                          _ interface: repeat (each SecondaryInterface).Type)
    throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  try CoCreateInstanceEx(CLSID(clsid), activation: context, server: server,
                         requesting: primary, repeat each interface)
}

@_alwaysEmitIntoClient
public func CoCreateInstanceEx<Implementation, Primary, each SecondaryInterface>(_ implementation: Implementation.Type,
                                                                                 activation context: CLSCTX = COMActivationOptions.current.context,
                                                                                 server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                                                 requesting primary: Primary.Type,
                                                                                 _ interfaces: repeat (each SecondaryInterface).Type)
    throws(COMError) -> (Primary?, repeat (each SecondaryInterface)?)
    where Implementation.Type: COMActivatable, Primary.Type: COMInterface,
        repeat (each SecondaryInterface).Type: COMInterface {
  try CoCreateInstanceEx(Implementation.CLSID, activation: context, server: server,
                         requesting: primary, repeat each interfaces)
}

#endif // $_MicrosoftCOM
