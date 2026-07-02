//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(Windows)
import WinSDK
#endif

/// An error thrown when a COM method returns a failing `HRESULT`.
///
/// When the synthesised HRESULT-to-`throws` wrapper detects a failing result code, it
/// captures the thread-local `IErrorInfo` (if available) and throws a `COMError` populated
/// with the HRESULT and the error information fields. If no `IErrorInfo` is set, the
/// optional fields are `nil` and only the `hresult` is available.
///
/// When a Swift `@com` method throws, the synthesised wrapper populates the thread-local
/// `IErrorInfo` from the error before returning the HRESULT to the COM caller. If the
/// thrown error is a `COMError`, its `hresult` is returned directly and its fields are
/// used to populate `IErrorInfo`. Other Swift errors map to `E_FAIL` with
/// `localizedDescription` as the error message.
///
/// ```swift
/// do {
///     try comObject.doWork()
/// } catch let error as COMError {
///     print(error)  // "0x80004005: The operation failed"
/// }
/// ```
@frozen
public struct COMError: Error {
  /// The failing HRESULT value.
  public let hresult: HRESULT

  /// The error message from `IErrorInfo::GetDescription`, or `nil` if
  /// no `IErrorInfo` was set by the callee.
  public let message: String?

  /// The source component name from `IErrorInfo::GetSource`, or `nil`.
  public let source: String?

  /// Help information from `IErrorInfo`.
  ///
  /// `file` is the help file path from `IErrorInfo::GetHelpFile`, or `nil`.
  /// `context` is the help context identifier from `IErrorInfo::GetHelpContext`.
  public let help: (file: String?, context: UInt32)

  internal init(hr: HRESULT, info: (any IErrorInfo)? = nil) {
    self.hresult = hr
    if let info {
      self.message = try? info.description
      self.source = try? info.source
      self.help = (try? info.helpFile, (try? info.helpContext) ?? 0)
    } else {
      self.message = nil
      self.source = nil
      self.help = (nil, 0)
    }
  }
}

extension COMError: CustomStringConvertible {
  /// A human-readable description of the error.
  ///
  /// If a message was captured from `IErrorInfo`, the description includes both
  /// the HRESULT and the message. Otherwise, only the HRESULT is shown.
  @inlinable
  public var description: String {
    if let message { "\(hresult): \(message)" } else { "\(hresult)" }
  }
}

extension COMError: Sendable {
}
