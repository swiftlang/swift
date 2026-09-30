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

/// # COM Activation
///
/// Type-safe activation of COM classes over `CoCreateInstance` and
/// `CoCreateInstanceEx`. The public surface is shaped so that only valid
/// activations can be written: every parameter combination COM would reject at
/// runtime should be unrepresentable.
///
/// ## Design
///
/// The overloads are derived from the *constraints* of COM activation rather
/// than from the raw Win32 parameter list. The dangerous forms — aggregation
/// with an arbitrary interface, activation with zero interfaces, aggregation on
/// a remote server — are not available. Each public entry point is
/// a point in the activation space that is always valid. The activation context
/// and server travel as one ``COMActivationOptions`` value, so inheriting the
/// ambient configuration requires one lookup and the correct call remains the
/// shortest one.
///
/// ## Axes of activation
///
/// - term Class identity: a ``CLSID``, a `WinSDK.GUID`, or an activatable Swift
///   type conforming to ``COMActivatable``.
/// - term Arity: one interface, or several requested in a single activation.
/// - term Server: in-process/local by default, or a specific, possibly remote
///   host via ``COMServerInfo``.
/// - term Aggregation: composing the new instance into an outer `IUnknown`.
///
/// The class-identity axis is handled by overloading and the server axis by the
/// activation options; arity and aggregation change the *shape* of the result,
/// so they select distinct entry points.
///
/// ## Entry points
///
/// ### One interface — ``CoCreateInstance(_:activation:as:)``
///
/// Prefer this whenever you need a single interface. It returns the interface
/// directly and throws ``COMError`` if activation or the implied
/// `QueryInterface` fails. Overloaded for ``CLSID``, `GUID`, and
/// ``COMActivatable`` implementations; the `GUID` form is a transparent bridge
/// to the `CLSID` form. `activation` defaults from
/// ``COMActivationOptions/current``, so a bare call inherits one snapshot of
/// the ambient configuration.
///
/// ### Aggregation — ``CoCreateInstance(_:activation:aggregating:)``
///
/// Use this when composing the instance into an outer object. It returns
/// `any IUnknown`, and only `IUnknown`, because `IClassFactory::CreateInstance`
/// requires the requested IID to be `IID_IUnknown` while aggregating. There is
/// deliberately no `as:` parameter — requesting any other interface during
/// aggregation is invalid, so it is made unspellable rather than rejected at
/// runtime — and no `server:` parameter, because aggregation is in-process only
/// and cannot be combined with a remote server.
///
/// ### Several interfaces — ``CoCreateInstanceEx(_:activation:requesting:_:)``
///
/// Use this when you need more than one interface on the instance in a single
/// activation round-trip — the reason `MULTI_QI` exists, and worth the most
/// against a remote server. It returns a tuple of optionals: each requested
/// interface is independently optional because a per-interface `QueryInterface`
/// may fail with `E_NOINTERFACE` while the activation as a whole succeeds. A
/// thrown ``COMError`` means the activation call itself failed; a `nil` element
/// means that one interface was unavailable. The signature takes a mandatory
/// `first` interface followed by a variadic pack, so a zero-interface request —
/// which COM rejects with `E_INVALIDARG` — cannot be written.
///
/// ## What you cannot spell, and why
///
/// - Aggregation with a specific interface: the aggregating overload returns
///   `any IUnknown` only. `IClassFactory::CreateInstance` mandates
///   `IID_IUnknown` when aggregating.
/// - Aggregation with a server: no overload accepts both `aggregating:` and
///   `server:`. A remote aggregate is meaningless — aggregation is in-process.
/// - An empty request to ``CoCreateInstanceEx``: `first` is mandatory, so the
///   `E_INVALIDARG` case is unrepresentable.
/// - A single interface through ``CoCreateInstanceEx``: not exposed, because it
///   is identical in effect to ``CoCreateInstance(_:activation:as:)``.
///   The single-interface `Ex` path exists only as an internal funnel, used
///   for remote activation.
///
/// ## Choosing an entry point
///
/// - One interface, local or remote → ``CoCreateInstance(_:activation:as:)``
/// - Several interfaces at once → ``CoCreateInstanceEx(_:activation:requesting:_:)``
/// - Aggregating into an outer → ``CoCreateInstance(_:activation:aggregating:)``
/// - A Swift ``COMActivatable`` type → the `Implementation` overload of either
///
/// - SeeAlso: The asynchronous model, documented alongside the `async`
///   overloads.

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
                                         activation context: CLSCTX,
                                         server: consuming COMServerInfo?,
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
        ManagedObject<IUnknown>.passUnretained($0)
            .assumingMemoryBound(to: WinSDK.IUnknown.self)
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

@_alwaysEmitIntoClient
package func CoCreateInstanceEx(_ clsid: CLSID,
                                activation context: CLSCTX,
                                server: consuming COMServerInfo? = nil,
                                interface: borrowing IID)
    throws(COMError) -> WinSDK.MULTI_QI {
  var result = WinSDK.MULTI_QI()
  let hr = withUnsafePointer(to: interface) { interface in
    result.pIID = UnsafeRawPointer(interface)
        .assumingMemoryBound(to: WinSDK.IID.self)
    return withUnsafePointer(to: clsid) {
      let rclsid =
          UnsafeRawPointer($0).assumingMemoryBound(to: WinSDK.CLSID.self)
      return withUnsafeMutablePointer(to: &result) { result in
        guard let server else {
          return WinSDK.CoCreateInstanceEx(rclsid, nil, DWORD(context.rawValue),
                                           nil, 1, result)
        }

        return server.name.withCString(encodedAs: UTF16.self) { lpszServerName in
          var info = COSERVERINFO()
          info.pwszName = UnsafeMutablePointer(mutating: lpszServerName)
          return WinSDK.CoCreateInstanceEx(rclsid, nil, DWORD(context.rawValue),
                                           &info, 1, result)
        }
      }
    }
  }

  guard SUCCEEDED(hr) || hr == E_NOINTERFACE else {
    throw COMError(hr: hr)
  }
  guard SUCCEEDED(result.hr) else {
    throw COMError(hr: result.hr)
  }
  result.pIID = nil
  return result
}

@_alwaysEmitIntoClient
package func CoCreateInstanceEx<Interface>(_ clsid: borrowing CLSID,
                                           activation context: CLSCTX,
                                           server: consuming COMServerInfo? = nil,
                                           as interface: Interface.Type = Interface.self)
    throws(COMError) -> Interface where Interface.Type: COMInterface {
  let result = try CoCreateInstanceEx(clsid, activation: context,
                                      server: server, interface: Interface.IID)
  guard let pointer = result.pItf else {
    throw COMError(hr: E_UNEXPECTED)
  }
  return ManagedObject<Interface>.takeRetainedValue(UnsafeMutableRawPointer(pointer))
}

// MARK: - CoCreateInstance

@_alwaysEmitIntoClient
public func CoCreateInstance<Interface>(_ clsid: borrowing CLSID,
                                        activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                        as: Interface.Type = Interface.self)
    throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try CoCreateInstance(clsid, activation: options.context,
                              server: options.server,
                              outer: nil, as: Interface.self)
}

@_transparent
public func CoCreateInstance<Interface>(_ clsid: borrowing WinSDK.GUID,
                                        activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                        as: Interface.Type = Interface.self)
    throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try CoCreateInstance(CLSID(clsid), activation: options,
                              as: Interface.self)
}

@_transparent
public func CoCreateInstance<Implementation, Interface>(_ implementation: Implementation.Type,
                                                        activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                        as: Interface.Type = Interface.self)
    throws(COMError) -> Interface
    where Implementation.Type: COMActivatable, Interface.Type: COMInterface {
  return try CoCreateInstance(Implementation.CLSID, activation: options,
                              as: Interface.self)
}

@_alwaysEmitIntoClient
public func CoCreateInstance(_ clsid: borrowing CLSID,
                             activation context: CLSCTX = COMActivationOptions.current.context,
                             aggregating outer: any IUnknown)
    throws(COMError) -> any IUnknown {
  return try CoCreateInstance(clsid, activation: context, server: nil,
                              outer: outer, as: IUnknown.self)
}

@_transparent
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
                                                                           activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                                           requesting primary: PrimaryInterface.Type,
                                                                           _ interfaces: repeat (each SecondaryInterface).Type)
    throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  let context = options.context
  let server = options.server
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

      let hr = iids.span.withUnsafeBufferPointer { interfaces in
        return withUnsafePointer(to: clsid) {
          let rclsid = UnsafeRawPointer($0)
              .assumingMemoryBound(to: WinSDK.CLSID.self)
          for index in results.indices {
            let iid = interfaces.baseAddress!.advanced(by: index)
            results[index].pIID = UnsafeRawPointer(iid)
                .assumingMemoryBound(to: WinSDK.IID.self)
          }

          guard let server else {
            return WinSDK.CoCreateInstanceEx(rclsid, nil, DWORD(context.rawValue),
                                             nil, DWORD(results.count),
                                             results.baseAddress!)
          }

          return server.name.withCString(encodedAs: UTF16.self) { lpszServerName in
            var info = COSERVERINFO()
            info.pwszName = UnsafeMutablePointer(mutating: lpszServerName)
            return WinSDK.CoCreateInstanceEx(rclsid, nil, DWORD(context.rawValue),
                                             &info, DWORD(results.count),
                                             results.baseAddress!)
          }
        }
      }
      if SUCCEEDED(hr) || hr == E_NOINTERFACE {
        for index in results.indices where FAILED(results[index].hr) {
          results[index].pItf = nil
        }
      } else {
        failure = hr
      }

      var iterator = results[...]
      let primary =
          ManagedObject<PrimaryInterface>.takeRetainedValue(iterator.removeFirst().pItf)
      return (primary, repeat ManagedObject<each SecondaryInterface>.takeRetainedValue(iterator.removeFirst().pItf))
    }
  }

  if let failure {
    throw COMError(hr: failure)
  }
  return instances
}

@_transparent
public func CoCreateInstanceEx<PrimaryInterface, each SecondaryInterface>(_ clsid: borrowing WinSDK.GUID,
                                                                          activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                                          requesting primary: PrimaryInterface.Type,
                                                                          _ interface: repeat (each SecondaryInterface).Type)
    throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  try CoCreateInstanceEx(CLSID(clsid), activation: options,
                         requesting: primary, repeat each interface)
}

@_transparent
public func CoCreateInstanceEx<Implementation, Primary, each SecondaryInterface>(_ implementation: Implementation.Type,
                                                                                 activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                                                 requesting primary: Primary.Type,
                                                                                 _ interfaces: repeat (each SecondaryInterface).Type)
    throws(COMError) -> (Primary?, repeat (each SecondaryInterface)?)
    where Implementation.Type: COMActivatable, Primary.Type: COMInterface,
        repeat (each SecondaryInterface).Type: COMInterface {
  try CoCreateInstanceEx(Implementation.CLSID, activation: options,
                         requesting: primary, repeat each interfaces)
}

#endif // $_MicrosoftCOM
