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

/// A reference held across a suspension point must be callable from whatever
/// cooperative-pool thread the task resumes on.

private import COM
public import WinSDK

@_alwaysEmitIntoClient
public func CoCreateInstance<Interface>(_ clsid: COM.CLSID,
                                        activation context: COM.CLSCTX = COMActivationOptions.current.context,
                                        server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                        as interface: Interface.Type = Interface.self)
    async throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try COM.CoCreateInstance(clsid, activation: context, server: server,
                                  as: interface)
}

@_transparent
@_alwaysEmitIntoClient
public func CoCreateInstance<Interface>(_ clsid: WinSDK.GUID,
                                        activation context: COM.CLSCTX = COMActivationOptions.current.context,
                                        server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                        as interface: Interface.Type = Interface.self)
    async throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try await CoCreateInstance(CLSID(clsid), activation: context, server: server,
                                    as: interface)
}

@_alwaysEmitIntoClient
public func CoCreateInstance<Implementation, Interface>(_ implementation: Implementation.Type,
                                                        activation context: COM.CLSCTX = COMActivationOptions.current.context,
                                                        server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                        as interface: Interface.Type = Interface.self)
    async throws(COMError) -> Interface
    where Implementation.Type: COMActivatable, Interface.Type: COMInterface {
  return try await CoCreateInstance(Implementation.CLSID, activation: context, server: server,
                                    as: interface)
}

@_alwaysEmitIntoClient
public func CoCreateInstanceEx<PrimaryInterface, each SecondaryInterface>(_ clsid: COM.CLSID,
                                                                          activation context: COM.CLSCTX = COMActivationOptions.current.context,
                                                                          server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                                          requesting first: PrimaryInterface.Type,
                                                                          _ interfaces: repeat (each SecondaryInterface).Type)
    async throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  return try COM.CoCreateInstanceEx(clsid, activation: context, server: server,
                                    requesting: first, repeat each interfaces)
}

@_transparent
@_alwaysEmitIntoClient
public func CoCreateInstanceEx<PrimaryInterface, each SecondaryInterface>(_ clsid: WinSDK.GUID,
                                                                          activation context: COM.CLSCTX = COMActivationOptions.current.context,
                                                                          server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                                          requesting first: PrimaryInterface.Type,
                                                                          _ interfaces: repeat (each SecondaryInterface).Type)
    async throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  return try await CoCreateInstanceEx(CLSID(clsid), activation: context,
                                      server: server,
                                      requesting: first, repeat each interfaces)
}

@_alwaysEmitIntoClient
public func CoCreateInstanceEx<Implementation, PrimaryInterface, each SecondaryInterface>(_ implementation: Implementation.Type,
                                                                                          activation context: COM.CLSCTX = COMActivationOptions.current.context,
                                                                                          server: consuming COMServerInfo? = COMActivationOptions.current.server,
                                                                                          requesting first: PrimaryInterface.Type,
                                                                                          _ interfaces: repeat (each SecondaryInterface).Type)
    async throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where Implementation.Type: COMActivatable,
        PrimaryInterface.Type: COMInterface,
        repeat (each SecondaryInterface).Type: COMInterface {
  return try await CoCreateInstanceEx(Implementation.CLSID, activation: context,
                                      server: server,
                                      requesting: first, repeat each interfaces)
}

#endif
