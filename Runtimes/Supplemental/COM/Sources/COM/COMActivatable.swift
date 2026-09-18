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

/// Identifies a Swift COM class that supports activation.
///
/// The metatype of a `@com` class with an activation identity conforms to
/// `COMActivatable`, allowing activation APIs to obtain the identity used by
/// the selected COM model.
///
/// This conformance is automatic and cannot be written explicitly.
public protocol COMActivatable {
#if $_MicrosoftCOM
  /// The CLSID of this class.
  var CLSID: CLSID { get }
#endif // $_MicrosoftCOM
}
