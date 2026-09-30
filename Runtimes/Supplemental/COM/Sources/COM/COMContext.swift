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

@frozen
public struct COINIT: OptionSet, Sendable {
  public let rawValue: UInt32

  @_transparent
  public init(rawValue: UInt32) {
    self.rawValue = rawValue
  }

  /// Initialize the thread as part of the MTA.
  @_transparent
  public static var multithreaded: COINIT { COINIT(rawValue: 0) }

  /// Initialize the thread as an STA.
  @_transparent
  public static var apartment: COINIT { COINIT(rawValue: 0x2) }

  /// Disable obsolete OLE 1 DDE support.
  @_transparent
  public static var NDDE: COINIT { COINIT(rawValue: 0x4) }

  /// Prefer speed over memory use.
  @_transparent
  public static var speed: COINIT { COINIT(rawValue: 0x8) }
}

extension COINIT {
  /// MTA initialization with obsolete OLE 1 DDE disabled.
  @_transparent
  public static var `default`: COINIT { .NDDE }
}

/// Manages COM initialisation on the current thread with explicit lifetime.
///
/// `COMContext` calls `CoInitializeEx` at construction and `CoUninitialize` at
/// deallocation. It is suited to long-lived programs where wrapping the entire
/// body in a `withCOMContext` closure is impractical.
///
/// ```swift
/// func main() throws {
///     let com = try COMContext(.apartment)
///
///     let voice = try SpVoice()
///     try voice.speak("Hello")
///     processEvents()
///     // ...
/// }
/// // CoUninitialize called when `com` goes out of scope
/// ```
///
/// For scoped usage, prefer `withCOMContext(_:activation:server:_:)`. For
/// entry-point programs, prefer `@COMMain`.
///
/// `COMContext` is non-copyable because `CoInitializeEx`/`CoUninitialize` are
/// paired per-thread and must not be duplicated.
public struct COMContext: ~Copyable {
  /// Initialises COM on the current thread with the specified threading model.
  ///
  /// - Parameter options: The apartment threading model for this thread.
  /// - Throws: If `CoInitializeEx` fails (e.g., the thread is already initialised
  ///   with a different threading model).
  @_alwaysEmitIntoClient
  public init(_ options: COINIT) throws(COMError) {
    let hr = CoInitializeEx(nil, DWORD(options.rawValue))
    guard hr >= 0 else { throw COMError(hr: hr) }
  }

  @_alwaysEmitIntoClient
  deinit {
    CoUninitialize()
  }
}

/// Runs a synchronous operation in an initialized COM apartment.
///
/// Activation operations made during `body` inherit the context and server.
/// Initialization and activation are thread-bound, so this scope cannot cross
/// an `await`.
@_alwaysEmitIntoClient
public func withCOMContext<Result: ~Copyable>(_ options: COINIT = .default,
                                              activation: CLSCTX = .all,
                                              server: consuming COMServerInfo? = nil,
                                              _ body: () throws -> Result) throws -> Result {
  let context = try COMContext(options)
  let options = COMActivationOptions(context: activation, server: server)
  return try withExtendedLifetime(context) {
    if kActivationOptionsKey == TLS_OUT_OF_INDEXES {
      throw COMError(hr: E_OUTOFMEMORY)
    }
    return try withUnsafePointer(to: options) { options in
      let previous = TlsGetValue(kActivationOptionsKey)
      guard TlsSetValue(kActivationOptionsKey,
                        UnsafeMutableRawPointer(mutating: options)) else {
        throw COMError(hr: HRESULT_FROM_WIN32(GetLastError()))
      }
      defer { _ = TlsSetValue(kActivationOptionsKey, previous) }
      return try body()
    }
  }
}

#endif // $_MicrosoftCOM
