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

/// Rich error information for a failed COM `HRESULT`.
///
/// `IErrorInfo` is the standard COM interface for providing human-readable
/// error details alongside a failing `HRESULT`. When a COM method fails, the
/// synthesised wrapper captures the thread-local `IErrorInfo` into a
/// `COMError`.
///
/// On Windows, `IErrorInfo` is imported from the SDK headers. On non-Windows
/// platforms, the `COM` module defines it directly. The vtable layout matches
/// the COM specification: `IUnknown` slots 0-2, then `GetGUID` (3),
/// `GetSource` (4), `GetDescription` (5), `GetHelpFile` (6),
/// `GetHelpContext` (7).
///
/// Classes that opt into `ISupportErrorInfo` must also conform to `IErrorInfo`
/// to provide the error detail fields that `ISupportErrorInfo` advertises.
@com(interface: "1CF2B120-547D-101B-8E65-08002B2BD119")
public protocol IErrorInfo: IUnknown {
  /// The GUID of the interface that defined the error.
  var guid: IID { get throws }

  /// The programmatic identifier of the component that raised the error.
  var source: String { get throws }

  /// A human-readable description of the error.
  var description: String { get throws }

  /// The path to the help file that describes the error.
  var helpFile: String { get throws }

  /// The help context identifier within the help file.
  var helpContext: UInt32 { get throws }
}
