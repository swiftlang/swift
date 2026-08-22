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

internal import WinSDK

internal let kActivationOptionsKey: DWORD = TlsAlloc()

// FIXME(compnerd): we should prefer WinSDK's definitions canonically typed on
// Windows.

/// The class context flags controlling which server types `CoCreateInstance`
/// considers.
///
/// These flags filter the activation scope. The COM runtime searches the
/// registry for server entries matching the requested CLSID and the allowed
/// context flags.
@frozen
public struct CLSCTX: OptionSet, Sendable {
  public let rawValue: UInt32

  public init(rawValue: UInt32) {
    self.rawValue = rawValue
  }

  /// In-process DLL server (`CLSCTX_INPROC_SERVER`).
  public static let inproc  = CLSCTX(rawValue: 0x1)
  /// In-process OLE handler (`CLSCTX_INPROC_HANDLER`).
  public static let handler = CLSCTX(rawValue: 0x2)
  /// Out-of-process EXE on the local machine (`CLSCTX_LOCAL_SERVER`).
  public static let local   = CLSCTX(rawValue: 0x4)
  /// DCOM remote machine (`CLSCTX_REMOTE_SERVER`).
  public static let remote  = CLSCTX(rawValue: 0x10)
  /// Any available server.
  public static let all: CLSCTX = [.inproc, .handler, .local, .remote]
}

/// Options controlling how COM objects are activated via `CoCreateInstance`.
///
/// A `withCOMContext` scope supplies the class context inherited by calls to
/// `CoCreateInstance`. The default is `.all`, meaning `CoCreateInstance` will
/// search all available server types on the local machine.
///
/// Most code never constructs this type directly. Instead, use
/// `withCOMContext` to scope COM initialize and activation context together:
///
/// ```swift
/// try withCOMContext(activation: .inproc) {
///     let obj = try CImplementation()  // activates in-process only
/// }
/// ```
///
/// For DCOM remote activation, pass a `COMServerInfo`:
///
/// ```swift
/// try await withCOMContext(activation: .remote, server: COMServerInfo(name: "host.example.com")) {
///     let obj = try CImplementation()  // activates on remote machine
/// }
/// ```
public struct COMActivationOptions {
  /// The class context flags controlling which server types are considered.
  public var context: CLSCTX

  /// The remote server to activate on, or `nil` for local activation.
  public var server: COMServerInfo?

  public init(context: CLSCTX, server: consuming COMServerInfo? = nil) {
    self.context = context
    self.server = server
  }
}

extension COMActivationOptions {
  /// The default activation options: `.all` context, no remote server.
  public static let `default` = COMActivationOptions(context: .all)
}

extension COMActivationOptions: Sendable {
}

extension COMActivationOptions {
  @usableFromInline
  internal static var current: COMActivationOptions {
    if kActivationOptionsKey == TLS_OUT_OF_INDEXES {
      return COMActivationOptions.default
    }
    guard let value = TlsGetValue(kActivationOptionsKey) else {
      return COMActivationOptions.default
    }
    return value.assumingMemoryBound(to: COMActivationOptions.self).pointee
  }
}

#endif // $_MicrosoftCOM
