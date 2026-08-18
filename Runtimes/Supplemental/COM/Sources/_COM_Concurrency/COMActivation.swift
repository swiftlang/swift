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

/// # COM Activation — Asynchronous Model
///
/// The `async` overloads mirror the synchronous surface for use from Swift
/// concurrency, with one categorical difference: they operate only in the
/// multithreaded apartment (MTA).
///
/// ## Why `async` implies the MTA
///
/// A task can suspend at any `await` and resume on a different cooperative-pool
/// thread. COM's apartment models react to that migration very differently:
///
/// - term Single-threaded apartment (STA): objects have thread affinity — a
///   reference is valid only on the thread and apartment that created it. Held
///   across a suspension point, it is called from the wrong thread on resume,
///   which is undefined; a correct cross-apartment call needs marshaling that a
///   raw interface pointer does not perform.
/// - term Multithreaded apartment (MTA): a single apartment shared by every MTA
///   thread in the process. A reference produced there — a free-threaded
///   pointer, or a marshaled proxy to a host STA — is callable from any MTA
///   thread, and with an implicit MTA established every cooperative-pool worker
///   acts as an MTA member.
///
/// An interface activated in the MTA therefore survives task migration; one
/// activated in an STA does not. The async surface is MTA-only, and the STA is
/// not expressible on it.
///
/// ## Entry and establishment — ``withCOMContext(activation:server:_:)``
///
/// Enter the async COM context with `try await withCOMContext { … }`. It
/// publishes the activation options as a task-local and keeps an implicit MTA
/// alive for the body's duration via `CoIncrementMTAUsage`.
///
/// - Important: It does not call `CoInitializeEx` and does not enter an
///   apartment. `CoInitializeEx`/`CoUninitialize` are thread-affine and must
///   balance on one thread; across an `await` the init and the uninit would
///   land on different threads — leaking the first, mismatching the second.
///   `CoIncrementMTAUsage` has no thread affinity — its cookie may be released
///   on any thread — which is precisely why it, and not apartment
///   initialization, is the async establishment primitive.
///
/// The context is reference-counted and nests cleanly: an inner
/// `withCOMContext` under ``COMMain`` or another enclosing context is a harmless
/// increment over a context that never drops.
///
/// ## The async surface
///
/// The `async` overloads of ``CoCreateInstance`` and ``CoCreateInstanceEx``
/// match their synchronous counterparts in shape and defaults, with three
/// deliberate omissions:
///
/// - No apartment parameter — async is MTA-only, and the STA cannot be named
///   because it cannot survive migration.
/// - No aggregation — it is in-process, synchronous, and apartment-affine, so it
///   stays on ``CoCreateInstance(_:activation:aggregating:)``.
/// - No single-interface `Ex` — single-interface activation is
///   ``CoCreateInstance``; `Ex` is strictly the multi-interface form.
///
/// Results are ordinary escapable interfaces whose lifetime is governed by their
/// own reference count, not by the `withCOMContext` scope. That is correct under
/// a process-lifetime context such as ``COMMain``, where the MTA outlives any
/// single call.
///
/// - Warning: When `withCOMContext` is the *only* thing keeping the MTA alive,
///   an interface must not outlive that scope: once the context releases its
///   `CoIncrementMTAUsage`, an escaped object has no live apartment behind it.
///   Under ``COMMain`` or an enclosing context this cannot happen; for a bounded
///   region with no such backstop, keep activated objects inside the scope.
///
/// ## Blocking
///
/// Activation can block — spawning a local server, or a network round-trip to a
/// remote one. All async activation routes through a single seam that relies on
/// the ambient MTA and is the one place such work may be moved off the
/// cooperative pool; in-process activation can run inline.
///
/// ## When to prefer `async`
///
/// - Use the async surface when activation may block and you are already within
///   an MTA context (``COMMain`` or `withCOMContext`).
/// - Use the *synchronous* `withCOMContext`, which can enter an STA, when you
///   need apartment-threaded objects — many UI and shell interfaces. Those
///   cannot be activated or held on the async surface.
///
/// - SeeAlso: The synchronous activation model.

private import COM
public import WinSDK

@_transparent
public func CoCreateInstance<Interface>(_ clsid: borrowing COM.CLSID,
                                        activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                        as interface: Interface.Type = Interface.self)
    async throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try COM.CoCreateInstance(clsid, activation: options,
                                  as: interface)
}

@_transparent
public func CoCreateInstance<Interface>(_ clsid: borrowing WinSDK.GUID,
                                        activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                        as interface: Interface.Type = Interface.self)
    async throws(COMError) -> Interface where Interface.Type: COMInterface {
  return try await CoCreateInstance(CLSID(clsid), activation: options,
                                    as: interface)
}

@_transparent
public func CoCreateInstance<Implementation, Interface>(_ implementation: Implementation.Type,
                                                        activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                        as interface: Interface.Type = Interface.self)
    async throws(COMError) -> Interface
    where Implementation.Type: COMActivatable, Interface.Type: COMInterface {
  return try await CoCreateInstance(Implementation.CLSID, activation: options,
                                    as: interface)
}

@_transparent
public func CoCreateInstanceEx<PrimaryInterface, each SecondaryInterface>(_ clsid: borrowing COM.CLSID,
                                                                          activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                                          requesting first: PrimaryInterface.Type,
                                                                          _ interfaces: repeat (each SecondaryInterface).Type)
    async throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  return try COM.CoCreateInstanceEx(clsid, activation: options,
                                    requesting: first, repeat each interfaces)
}

@_transparent
public func CoCreateInstanceEx<PrimaryInterface, each SecondaryInterface>(_ clsid: borrowing WinSDK.GUID,
                                                                          activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                                          requesting first: PrimaryInterface.Type,
                                                                          _ interfaces: repeat (each SecondaryInterface).Type)
    async throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where PrimaryInterface.Type: COMInterface, repeat (each SecondaryInterface).Type: COMInterface {
  return try await CoCreateInstanceEx(CLSID(clsid), activation: options,
                                      requesting: first, repeat each interfaces)
}

@_transparent
public func CoCreateInstanceEx<Implementation, PrimaryInterface, each SecondaryInterface>(_ implementation: Implementation.Type,
                                                                                          activation options: consuming COMActivationOptions = COMActivationOptions.current,
                                                                                          requesting first: PrimaryInterface.Type,
                                                                                          _ interfaces: repeat (each SecondaryInterface).Type)
    async throws(COMError) -> (PrimaryInterface?, repeat (each SecondaryInterface)?)
    where Implementation.Type: COMActivatable,
        PrimaryInterface.Type: COMInterface,
        repeat (each SecondaryInterface).Type: COMInterface {
  return try await CoCreateInstanceEx(Implementation.CLSID, activation: options,
                                      requesting: first, repeat each interfaces)
}

#endif
